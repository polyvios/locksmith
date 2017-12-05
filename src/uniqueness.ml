(*
 *
 * Copyright (c) 2004-2007, 
 *  Polyvios Pratikakis <polyvios@cs.umd.edu>
 *  Michael Hicks       <mwh@cs.umd.edu>
 *  Jeff Foster         <jfoster@cs.umd.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
(** Compute uniqueness information for the statements in a function *)

open Cil
open Pretty
open Lockutil
module E = Errormsg
module U = Uref
module DF = Dataflow
module IH = Inthash

let debug = ref false

type utype_kind =
    Unique
  | Shared

let d_ukind () (k: utype_kind) : doc =
  match k with
    Unique -> dprintf "unique"
  | Shared -> dprintf "shared"

(* each variable is mapped to one of these *)
type utype = (int U.uref * utype_kind)

(* state *)
type state = utype Strmap.t

let empty_state = Strmap.empty

let d_state () d =
  text "{" ++ align ++
  Strmap.fold
    (fun s (iuref, uk) d ->
      d ++ dprintf "%s: (%d, %a), " s (U.deref iuref) d_ukind uk)
    d
    nil
  ++ unalign ++ text "}"

let set_all_shared (i: int U.uref) (s: state) : state =
  Strmap.fold
    (fun n (i',u) s ->
      if (U.equal (i,i')) then Strmap.add n (i, Shared) s else s)
    s s

let merge_utype (i1,ut1: utype) (i2,ut2: utype) (s: state) : state =
  U.unify (fun (x, _) -> x) (i1,i2);
  match (ut1, ut2) with
  | Unique, Unique -> s
  | _,_ -> set_all_shared i1 s

let utype_equal (i1,ut1: utype) (i2, ut2: utype) : bool =
  (U.equal (i1,i2)) && (ut1 = ut2)

let merge_state (s1: state) (s2: state) : state =
  Strmap.fold
    (fun str t s -> let t' = Strmap.find str s in merge_utype t t' s)
    s1
    s2

let lookup (n : string) (s : state) : utype =
  try
    Strmap.find n s
  with Not_found -> (U.uref 0, Shared)

let set_shared (s: string) (t: state) : state =
  let i,ut = lookup s t in
  set_all_shared i t

let rec set_shared_exp (t: state) (e:exp) : state =
  match e with 
    Lval(Var(v),_) -> set_shared v.vname t
  | UnOp (_,e,_) -> set_shared_exp t e
  | BinOp (b,e1,e2,_) ->
      (match b with 
        PlusA | PlusPI | IndexPI | MinusA | MinusPI | MinusPP ->
          set_shared_exp (set_shared_exp t e1) e2
      | _ -> t)
  | CastE (_,e) -> set_shared_exp t e
  | AddrOf (Var(v),_) -> set_shared v.vname t
  | StartOf (Var(v),_) -> set_shared v.vname t
  | _ -> t

let utype_exp (e: exp) (state: state) : utype =
  match e with
    Lval(Var(v),NoOffset) -> lookup v.vname state
  | _ -> (U.uref 0, Shared)

module UT = struct

  let name = "uniqueness"
  let debug = debug

  type t = state

  let copy (t: t) : t = t

  (* For each statement in a function we keep the set of live variables. 
   * Indexed by statement id *)
  let stmtStartData: t IH.t = IH.create 10

  let pretty = d_state

  (* just set the state the first time *)
  let computeFirstPredecessor (s: stmt) (state: t) : t =
    state

  let combinePredecessors (s: stmt) ~(old: t) (input: t) : t option =
    let result = merge_state old input in
    if Strmap.equal utype_equal result old then None
    else Some result

  let doInstr (i: instr) (state: t) : t DF.action = 
    match i with
    | Set((Var(v1),NoOffset), Lval(Var(v2),NoOffset), _) ->
        let u1 = lookup v1.vname state in
        let u2 = lookup v2.vname state in
        let s = merge_utype u1 u2 state in
        DF.Done(s)
    | Set((Var(v),NoOffset), e, _) ->
        let t = typeOf e in
        let al = typeAttrs t in
        let is_alloc =
          (Strmap.mem v.vname !Locksettings.special_functions)
          && ((Strmap.find v.vname !Locksettings.special_functions) = Locksettings.Alloc)
        in
        if is_alloc || (hasAttribute "unique" al) then 
          (if !debug then
            ignore(E.log "assigning to %s a unique-attributed exp %a\n" v.vname d_exp e);
          DF.Done(state))
        else 
          let s' = set_shared v.vname state in
          let s'' = set_shared_exp s' e in
          DF.Done(s'')
    | Call(Some((Var(v),NoOffset)), e, el, _) ->
        let t = typeOf e in
        let s' = match unrollTypeDeep t with
          | TFun (t,_,_,_) ->
              let al = typeAttrs t in
              if hasAttribute "unique" al then begin
                if !debug then
                  ignore(E.log "trusting annotation that %a returns unique!\n"
                    d_exp e);
                state
              end
              else (set_shared v.vname state)
          | _ ->
            ignore(E.error "trying to call non-function type: %a\n" d_type t);
            assert false (* we can't call a non-function type *)
        in
        let outs = List.fold_left set_shared_exp s' el in
        DF.Done(outs)
    | Call(_, _, el, _) ->
        let outs = List.fold_left set_shared_exp state el in
        DF.Done(outs)
    | Asm(_, _, lvlist, _, _, _) ->
        let f in_state (_,_,lv) = set_shared_exp in_state (Lval(lv)) in
        let s = List.fold_left f state lvlist in
        DF.Done(s)
    | _ -> DF.Default

  let doStmt (s: stmt) (state: t) : t DF.stmtaction = DF.SDefault

  let doGuard _ _ = DF.GDefault

  let filterStmt _ = true
end

module Uniq = DF.ForwardsDataFlow(UT)

let options = [
  "--debug-uniqueness",
    Arg.Set(debug),
    " Print verbose status information for the uniqueness analysis.";
]

let is_unique (s: string) (st: state) : bool =
  let (_, u) = lookup s st in
  u = Unique

let compute_uniqueness (f: fundec) : unit =
  IH.clear UT.stmtStartData;
  let id = ref 1 in
  let state = List.fold_left
    (fun s v ->
      assert (not v.vglob); (* these are locals *)
      if v.vaddrof then s   (* never unique--too simple? *)
      else begin
        incr id;
        Strmap.add v.vname (U.uref !id, Unique) s
      end)
    Strmap.empty
    f.slocals
  in
  match f.sbody.bstmts with
    [] -> ()
  | start :: _ -> begin
      IH.add UT.stmtStartData start.sid state;
      Uniq.compute [start];
    end

let through_instr (state: state) (i: instr) : state =
  match (UT.doInstr i state) with
    DF.Done(s) -> s
  | DF.Default -> state
  | _ -> assert false

let get_stmt_state (s: stmt) : state =
  try
    IH.find UT.stmtStartData s.sid
  with Not_found -> empty_state

let forall_unique (f : string -> unit) (s: state) : unit =
  Strmap.iter (fun v (_,u) -> if u = Unique then f v) s
