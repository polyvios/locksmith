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
(** Compute live variables information for the statements in a function *)
(* I used dominators.ml as a template *)

open Cil
open Pretty
open Lockutil

module E = Errormsg
module DF = Dataflow
module IH = Inthash
module VS = Usedef.VS

let debug = ref false

let options = [
  "--debug-liveness",
    Arg.Set(debug),
    " Print verbose status information for the liveness analysis.";
]

(* I copied the following functions from Saffire.  Written by Mike Furr. *)
let rec vars_used_in_exp set e =
  match e with
  | Const _
  | SizeOf _
  | SizeOfE _
  | SizeOfStr _
  | AlignOf _
  | AlignOfE _ -> set
  | UnOp(_,exp,_) -> vars_used_in_exp set exp
  | BinOp(_,e1,e2,_) -> vars_used_in_exp (vars_used_in_exp set e1) e2
  | CastE(_,exp) -> vars_used_in_exp set exp
  | AddrOf (host,off)
  | StartOf (host,off)
  | Lval (host,off) ->
      let rec vars_in_off set = function
        | NoOffset
        | Field _ -> set
        | Index(exp,offset) -> vars_used_in_exp (vars_in_off set offset) exp
      in
      let hvars =
        match host with
        | Var(vi) -> VS.add vi set
        | Mem(exp) -> vars_used_in_exp set exp
      in
      vars_in_off hvars off

let live_vars_in_instr live_set instr =
  match instr with
  | Set((Var(v),NoOffset),exp,location) ->
      vars_used_in_exp (VS.remove v live_set) exp
  | Set((Var(v),_),exp,location) ->
      vars_used_in_exp live_set exp
  | Set((Mem(e),_),exp,location) ->
      vars_used_in_exp (vars_used_in_exp live_set exp) e

  | Call(Some(Var(v),NoOffset),exp,params,loc) ->
      let live_set = vars_used_in_exp (VS.remove v live_set) exp in
      List.fold_left vars_used_in_exp live_set params

  | Call(result,exp,params,loc) ->
      let live_set = match result with
        | None -> live_set
        | Some lval -> vars_used_in_exp live_set (Lval lval)
      in
      let live_set = vars_used_in_exp live_set exp in
      List.fold_left vars_used_in_exp live_set params

  | Asm _ -> live_set


(** Customization module for live variables *)
module LVT = struct
  let name = "livevars"
  let debug = debug 

  type t = VS.t

  let pretty () live_vars = 
    VS.fold (fun s d -> (text s.vname) ++ d) live_vars nil

   (** For each statement in a function we keep the set of live variables. 
    * Indexed by statement id *)
  let stmtStartData = IH.create 42

  let funcExitData = VS.empty

  let combineStmtStartData stmt ~(old) add =
    let ret = VS.union old add in
    if VS.equal ret old then None
    else Some ret

  let combineSuccessors s1 s2 = VS.union s1 s2
  
  let doInstr instr live_set =
    let live_set = live_vars_in_instr live_set instr in DF.Done(live_set)

  let doStmt stmt =
    match stmt.succs with
      [] ->
        let res' = 
         match stmt.skind with 
           Instr il -> 
             let handleInstruction i s = 
               currentLoc := get_instrLoc i;
               let action = doInstr i s in 
               match action with 
               | DF.Done s' -> s'
               | DF.Default -> s (* do nothing *)
               | DF.Post f -> f s
             in
             List.fold_right handleInstruction il VS.empty
         | Return (Some e, _) ->
             vars_used_in_exp VS.empty e
         | _ -> VS.empty
        in
        DF.Done res'
    | _ -> DF.Default

  let filterStmt _ _ = true
end

module LV = DF.BackwardsDataFlow(LVT)

let all_stmts = ref []

class liveVarsStmtVisitor : cilVisitor = object (self)
  inherit nopCilVisitor
  method vstmt stmt : stmt visitAction =
    IH.replace LVT.stmtStartData stmt.sid VS.empty;
    all_stmts := stmt :: !all_stmts;
    DoChildren
end

let computeLiveness (f: fundec) : unit = begin
  Inthash.clear LVT.stmtStartData;
  all_stmts := [];
  let lvvisitor = new liveVarsStmtVisitor in
  ignore(visitCilFunction lvvisitor f);
  LV.compute !all_stmts;
  all_stmts := [];
end

let get_stmt_live_vars stmt : VS.t =
  IH.find LVT.stmtStartData stmt.sid

let getLiveSet (sid: int) : VS.t option =
  try
    Some (IH.find LVT.stmtStartData sid)
  with Not_found -> None

(* for debug purposes. just calls computeLVars on every fundec in the file: *)
class liveVarsFunVisitor : cilVisitor = object (self)
  inherit nopCilVisitor
  method vfunc (fd: fundec) : fundec visitAction =
    computeLiveness fd;
    DoChildren
end

(* should be disabled by default, debug use only: *)
let feature : featureDescr =
  { fd_name = "livevars";
    fd_enabled = ref false;
    fd_description = "live variable sets";
    fd_extraopt = [];
    fd_doit = 
      (function (f: file) -> begin
        if !debug then
          ignore(E.log "live vars analysis starting...\n");
        let lvvisitor = new liveVarsFunVisitor in
          visitCilFileSameGlobals lvvisitor f;
        if !debug then
          ignore(E.log "live vars analysis done.\n");
      end);
    fd_post_check=false;
  }
 
