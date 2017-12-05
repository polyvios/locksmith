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
module Q = Queue
module E = Errormsg

open Labelflow
open Pretty

let debug = ref false
let no_linearity = ref false
let no_semiunification = ref false

let options = [
  "--debug-semiunification",
    Arg.Set(debug),
    " Print extra information about linearity effects.";

  "--no-linearity",
    Arg.Set(no_linearity),
    " Disable linearity checking in semiunification";

  "--no-semiunification",
    Arg.Set(no_semiunification),
    " Disable semiunification altogether";
]

(*****************************
 * types -- graph description
 *****************************)
type labels = lockSet * rhoSet

type epsilon_kind =
  | EEmpty
  | ESingleton of lock
  | EChi
  | EUplus of epsilon * epsilon

and epsilon = {
  e_id : int;
  e_kind : epsilon_kind;
  e_loc : Cil.location;
  mutable e_solution : LockSet.t option;
  mutable e_inst : instantiation list;
  mutable e_unified : epsilon option;
  mutable e_flow_to : epsilon list;
  mutable e_flow_from : epsilon list;
  mutable e_filter_to : (labels * epsilon) list;
  mutable e_inst_to : (instantiation * epsilon) list;
}

type inst = instantiation

type constr =
  Sub of epsilon * epsilon
| Filter of epsilon * labels * epsilon
| Inst of epsilon * instantiation * epsilon

(*
module Epsilon : Set.OrderedType with type t = epsilon =
  struct
    type t = epsilon
    let compare (e1: t) (e2: t) = e1.e_id - e2.e_id
  end
module ESet = Set.Make(Epsilon)

type eset = ESet.t
*)


let solved : bool ref = ref false

let epsilon_no : int ref = ref 0

(*********************
 * graph construction
 *********************)

let empty_epsilon : epsilon =
  {
    e_id = 0;
    e_kind = EEmpty;
    e_loc = Cil.locUnknown;
    e_solution = None;
    e_inst = [];
    e_unified = None;
    e_flow_to = [];
    e_flow_from = [];
    e_filter_to = [];
    e_inst_to = [];
  }

let all_epsilon : epsilon list ref = ref [ empty_epsilon ]

let make_epsilon (k: epsilon_kind) (il: instantiation list) : epsilon =
  if !no_semiunification then empty_epsilon
  else begin
    assert (not !solved);
    incr epsilon_no;
    let e = {
      e_id = !epsilon_no;
      e_kind = k;
      e_loc = !Cil.currentLoc;
      e_solution = None;
      e_inst = il;
      e_unified = None;
      e_flow_to = [];
      e_flow_from = [];
      e_filter_to = [];
      e_inst_to = [];
    } in
    all_epsilon := e::!all_epsilon;
    e
  end

let make_var_epsilon () : epsilon =
  make_epsilon EChi []

let singletonmap : epsilon LockHT.t = LockHT.create 10
let singleton (l: lock) : epsilon =
  assert(is_concrete_lock l);
  try
    LockHT.find singletonmap l
  with Not_found -> begin
    let e = make_epsilon (ESingleton l) [] in
    LockHT.add singletonmap l e;
    e
  end

let rec find_epsilon (e: epsilon) : epsilon =
  if (e == empty_epsilon) then begin
    assert (e.e_unified == None);
    empty_epsilon
  end else
  match e.e_unified with
    None -> e
  | Some e' ->
      let r = find_epsilon e' in
    assert (r.e_unified == None);
      e.e_unified <- Some r;
      r

let epsilon_equal e1 e2 =
  let e1 = find_epsilon e1 in
  let e2 = find_epsilon e2 in
  e1 == e2

(******************
 * pretty printing
 ******************)

let string_of_kind (k: epsilon_kind) : string =
  match k with
    EEmpty -> "empty"
  | ESingleton _ -> "{}"
  | EChi -> "x"
  | EUplus _ -> "+"

let d_epsilon () (e: epsilon) : doc = 
  let e = find_epsilon e in
  dprintf "e%d@@%a\\n%s" e.e_id Cil.d_loc e.e_loc
    (
      match e.e_kind with
        EEmpty -> "empty"
      | ESingleton l -> Printf.sprintf "{%s}" (dotstring_of_lock l)
      | EChi -> "x"
      | EUplus _ -> "+"
    )

(***************
 * graph output
 ***************)

let print_epsilon_struct_edges (outf: out_channel) (e: epsilon) : unit = begin
  match e.e_kind with
    EEmpty -> ()
  | ESingleton l ->
      ignore(fprintf outf "\"%s\" -> \"%a\" [style=\"dotted\"];\n"
                     (dotstring_of_lock l) d_epsilon e);
  | EChi -> ()
  | EUplus (e1,e2) ->
      ignore(fprintf outf "\"%a\" -> \"%a\" [style=\"dotted\"];\n"
                     d_epsilon e1 d_epsilon e);
      ignore(fprintf outf "\"%a\" -> \"%a\" [style=\"dotted\"];\n"
                     d_epsilon e2 d_epsilon e);
end

let print_epsilon_flow_edges (outf: out_channel) (e: epsilon) : unit = begin
  List.iter
    (fun e' ->
      ignore(fprintf outf "\"%a\" -> \"%a\";\n" d_epsilon e d_epsilon e')
    )
    e.e_flow_to;
  List.iter
    (fun (_,e') ->
      ignore(fprintf outf "\"%a\" -> \"%a\"[label=\"down\"];\n"
                     d_epsilon e d_epsilon e')
    )
    e.e_filter_to;
  List.iter
    (fun (i,e') ->
      ignore(fprintf outf "\"%a\" -> \"%a\"[label=\"%s\"];\n"
                     d_epsilon e d_epsilon e' (dotstring_of_inst i))
    )
    e.e_inst_to;
end

let print_graph (outf: out_channel) : unit = begin
  (* add "structure" edges *)
  List.iter (print_epsilon_struct_edges outf) !all_epsilon;
  (* add flow edges *)
  List.iter (print_epsilon_flow_edges outf) !all_epsilon;
end


(*****************
 * graph solution
 *****************)

let worklist : constr Q.t = Q.create ()

(* add a constraint e \leq x
 * returns false if the constraint was already there
 *)
let rec epsilon_flow (e: epsilon) (x: epsilon) : unit =
  if !no_semiunification then () else begin
    assert (not !solved);
    let e = find_epsilon e in
    let x = find_epsilon x in
            (*ignore(E.log "flow %a to %a\n" d_epsilon e d_epsilon x);*)
    if e == x then ()
    else if x.e_kind = EEmpty then (
      match e.e_kind with
        EEmpty -> ()
      | EUplus (e1, e2) ->
          epsilon_flow e1 x;
          epsilon_flow e2 x;
      | ESingleton l -> 
          if !debug then
            ignore(E.log "lock %a is non linear\n" d_lock l);
          set_nonlinear l
      | EChi -> unify_epsilon e x
    ) else if e.e_kind = EEmpty then () else (
      if not (List.exists (fun e' -> epsilon_equal e' x) e.e_flow_to)
      then ( 
        e.e_flow_to <- x::e.e_flow_to;
        x.e_flow_from <- e::x.e_flow_from;
        Q.add (Sub(e,x)) worklist
      )
    )
  end

(* add a constraint e \leq_l x (down-rule)
 * returns false if the constraint was already there
 *)
and epsilon_filter (e: epsilon) (l,r: labels) (x: epsilon) : unit =
  if !no_semiunification then () else begin
    assert (not !solved);
    let e = find_epsilon e in
    let x = find_epsilon x in
    if !debug then ignore(E.log "down edge %a -> %a on %a\n" d_epsilon e d_epsilon x d_lockset l);
    assert(x.e_kind = EEmpty);
    begin
      if not (LockSet.is_empty l || List.exists
           (fun ((l',r'),e') ->
             epsilon_equal e' x && LockSet.equal l l'
             && RhoSet.equal r r')
           e.e_filter_to)
      then (
        match e.e_kind with
          EEmpty -> ()
        | _ ->
            e.e_filter_to <- ((l,r),x)::e.e_filter_to;
            Q.add (Filter(e,(l,r),x)) worklist;
      )
    end
  end

and epsilon_inst (e: epsilon) (i: inst) (x: epsilon) : unit =
  if !no_semiunification then () else begin
    assert (not !solved);
    let e = find_epsilon e in
    let x = find_epsilon x in
    if x == empty_epsilon then epsilon_flow e x else begin
      assert (x.e_kind == EChi);
      try
        let (_, e') = List.find (fun (i',e') -> Inst.equal i i') e.e_inst_to in
        unify_epsilon e' x;
      with Not_found -> begin
        match e.e_kind with
          EEmpty -> ()
        | _ ->
            e.e_inst_to <- (i,x)::e.e_inst_to;
            Q.add (Inst(e,i,x)) worklist
      end;
    end
  end

and unify_epsilon (e1: epsilon) (e2: epsilon) : unit =
  if !no_semiunification then () else begin
  assert (not !solved);
  let e1 = find_epsilon e1 in
  let e2 = find_epsilon e2 in
  let e1, e2 = if e1.e_id < e2.e_id then e1, e2 else e2, e1 in
  if e1 == e2 then ()
  else begin
    if e1.e_kind <> EEmpty then (
      List.iter (fun e -> epsilon_flow e1 e) e2.e_flow_to;
      List.iter (fun (l,e) -> epsilon_filter e1 l e) e2.e_filter_to;
      List.iter (fun (i,e) -> epsilon_inst e1 i e) e2.e_inst_to;
      e1.e_inst <- e1.e_inst @ e2.e_inst;
    );

    e2.e_flow_to <- [];
    e2.e_filter_to <- [];
    e2.e_inst_to <- [];
    e2.e_inst <- [];
    e2.e_unified <- Some e1
  end
end

let union (e1: epsilon) (e2: epsilon) : epsilon =
  if !no_semiunification then empty_epsilon else
  if (e1 == empty_epsilon) then e2
  else if (e2 == empty_epsilon) then e1
  else begin
    let e = make_epsilon EChi [] in
    epsilon_flow e1 e;
    epsilon_flow e2 e;
    e
  end

let uplus (e1: epsilon) (e2: epsilon) : epsilon =
  if !no_semiunification then empty_epsilon else
  if !no_linearity then union e1 e2
  else begin
    if (e1 == empty_epsilon) then e2
    else if (e2 == empty_epsilon) then e1
    else make_epsilon (EUplus(e1,e2)) []
  end

let epsilon_global (e: epsilon) : unit =
  unify_epsilon e empty_epsilon

(* fig 8c, block 1 *)
let propagate_sub (e: epsilon) (x: epsilon) : unit = begin
  let e = find_epsilon e in
  let x = find_epsilon x in
  if !debug then ignore(E.log "flow %a to %a\n" d_epsilon e d_epsilon x);
  if e == empty_epsilon then () else begin
      List.iter
        (fun x' -> epsilon_flow e x')
        x.e_flow_to;
      List.iter
        (fun (l,x') -> epsilon_filter e l x')
        x.e_filter_to;
      List.iter
        (fun (i,x') -> epsilon_inst e i x')
        x.e_inst_to;
  end;
  if !debug then ignore(E.log "end flow\n");
end

let clone_epsilon (e: epsilon) (i: inst) : epsilon =
  let e = find_epsilon e in
  if (List.mem i e.e_inst) then begin
    epsilon_flow e empty_epsilon;
    empty_epsilon
  end
  else try
    let (_, e') = List.find (fun (i',e') -> Inst.equal i i') e.e_inst_to in
    e'
  with Not_found ->
    (*assert(not (List.mem i e.e_inst));*)
    let e' = make_epsilon EChi (i::e.e_inst) in
    epsilon_inst e i e';
    e'


(* fig 8c, block 2 *)
let propagate_inst (e: epsilon) (i: inst) (x: epsilon) : unit = begin
  let e = find_epsilon e in
  let x = find_epsilon x in
  if !debug then ignore(E.log "inst %a to %a through %a\n" d_epsilon e d_epsilon x d_instantiation i);
  if x == empty_epsilon then epsilon_flow e x else begin
    assert (x.e_kind = EChi);
    match e.e_kind with
      EEmpty -> ()
    | ESingleton l ->
        assert(is_concrete_lock l);
        let l' = clone_lock l i in
        let e' = singleton l' in
        (*e.e_inst_to <- (i,e')::e.e_inst_to;*)
        epsilon_flow e' x
    | EUplus (e1, e2) ->
        if (List.mem i e.e_inst) then epsilon_flow e empty_epsilon
        else begin
          let x1 = clone_epsilon e1 i in
          let x2 = clone_epsilon e2 i in
          let x3 = make_epsilon (EUplus(x1,x2)) (i::e.e_inst) in
          epsilon_flow x3 x;
        end
    | EChi -> List.iter (fun e' -> epsilon_inst e' i x) e.e_flow_from;
  end;
  if !debug then ignore(E.log "end inst\n");
end

let all_escapes : (lock * labels) list ref = ref []

(* fig 8c, block 3 *)
let propagate_filter (e: epsilon) (ls: labels) (x: epsilon) : unit =
begin
  let e = find_epsilon e in
  let x = find_epsilon x in
  let locks,rhos = ls in
  if !debug then ignore(E.log "filter %a to %a through %a\n" d_epsilon e d_epsilon x d_lockset locks);
  match e.e_kind with
    EEmpty -> ()
  | ESingleton l ->
      (* only propagate the "flows to" part of escapes *
       * the "correlates" part is checked post-mortem
       *)
      if LockSet.mem l (close_lockset locks)
      then epsilon_flow e x
      else all_escapes := (l,ls)::!all_escapes
  | EUplus (e1, e2) -> 
      assert(x.e_kind = EEmpty);
      (*
      let x1 = make_var_epsilon () in
      let x2 = make_var_epsilon () in
      epsilon_filter e1 ls x1;
      epsilon_filter e2 ls x2;
      let x3 = uplus x1 x2 in
      epsilon_flow x3 x;
      *)
      epsilon_filter e1 ls x;
      epsilon_filter e2 ls x;
  | EChi -> List.iter (fun e' -> epsilon_filter e' ls x) e.e_flow_from;
end

let solve () : unit =
  if !no_semiunification then () else begin
    assert (not !solved);
    let i = ref 1 in
    let rec loop () =
      let c = Q.take worklist in begin
        if !debug then ignore(E.log "iteration %d, worklist: %d\n" !i (Q.length worklist));
        match c with
          Sub (e, x) -> propagate_sub e x
        | Filter (e, l, x) -> propagate_filter e l x
        | Inst (e, i, x) -> propagate_inst e i x
      end;
      (*
      let name =(Printf.sprintf "graph%03d.dot" !i) in
      if !debug then begin
        Dotpretty.init_file name name;
          print_graph !Dotpretty.outf;
        Dotpretty.close_file ();
      end;
      *)
      incr i;
      loop ()
    in
    try
      loop ()
    with Q.Empty -> ();
    solved := true;
    LockHT.clear singletonmap;
  end

let check_escapes () : unit = begin
  List.iter
    (fun (l,(ls,rs)) -> 
      if Correlation.escapes l (ls,rs)
      then ignore(E.log "escapes check failed: %a through\n  %a\n%a\n"
                        d_lock l d_lockset ls d_rhoset rs))
    !all_escapes;
end

let rec solution (e: epsilon) : lockSet =
  assert (!solved);
  let e = find_epsilon e in
  match e.e_solution with
    Some ls -> ls
  | None -> 
      let sol = 
        match e.e_kind with
          EEmpty -> e.e_solution <- Some LockSet.empty;
            let d =
              List.fold_left
                (fun ls e' ->
                  ignore(E.log "%a flows to empty\n" d_epsilon e');
                  LockSet.union ls (solution e'))
                LockSet.empty
                e.e_flow_from
            in
            if !debug && not (LockSet.is_empty d) then ignore(E.log "nonlinear: %a\n" d_lockset d);
            LockSet.iter set_nonlinear d;
            LockSet.empty
        | ESingleton (l) -> concrete_lockset (get_lock_p2set l)
        | EChi ->
            e.e_solution <- Some LockSet.empty; (* are there cycles? *)
            List.fold_left
              (fun ls e' -> LockSet.union ls (solution e'))
              LockSet.empty
              e.e_flow_from
        | EUplus (e1, e2) ->
            let l1 = solution e1 in
            let l2 = solution e2 in
            let d = LockSet.inter l1 l2 in
            if !debug && not (LockSet.is_empty d) then ignore(E.log "nonlinear: %a\n" d_lockset d);
            LockSet.iter set_nonlinear d;
            LockSet.union l1 l2
     in
     e.e_solution <- Some sol;
     if !debug then ignore(E.log "solution of %a is %a\n" d_epsilon e d_lockset sol);
     sol


let check () : unit =
  if !no_semiunification then () else begin
    assert (!solved);
    List.iter (fun e -> ignore(solution e)) !all_epsilon;
    close_nonlinear();
    check_escapes ();
  end
