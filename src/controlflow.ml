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
open Pretty

module E = Errormsg
module LF = Labelflow
open Lockutil

let debug = ref false
let do_slice_phi = ref false
let do_postorder = ref true
let do_count_postorder = ref false

let options = [
  "--debug-controlflow",
     Arg.Set(debug),
     " Print control flow (phi graph) debugging output.";

  "--slice-phi",
     Arg.Set(do_slice_phi),
     " Optimize control flow graph, removing irrelevant nodes.";
  
  "--use-worklist",
     Arg.Clear(do_postorder),
     " Don't use postorder visit to fixpoint.  Instead, use a worklist to fixpoint for guarded-by.";

  "--count-postorder",
    Arg.Set(do_count_postorder),
    " Print the number of postorder visits of the phi graph.";
]

exception ControlFlowBug of string

type phi = {
  phi_id : int;
  phi_func : Cil.fundec option;
  phi_loc : Cil.location;
  phi_name : string;
  mutable phi_global : bool; (* true if it's global *)

  phi_times_visited: int ref;
    (* how many times was this node visited during solving *)

    (* It's a union-find structure on phis.
     * The edges here are only used during the analysis.
     * Here's how it works:
     *   Before the analysis, we are given a filter function
     *   that selects all phis that "matter" (usually based on kind).
     *   Then, we are collapsing all irrelevant nodes linked by
     *   non-parenthesis edges, while computing the effective edges for
     *   the unioned "conceptual" phi node that results from this.
     * currently not used.  TODO: add support for this.
     *)
  mutable phi_unified: phi option;
  mutable phi_next_tmp: phi list; (* successors *)
  mutable phi_prev_tmp: phi list; (* predecessors *)
  mutable phi_open_in_tmp: (LF.instantiation * phi) list; (* means p (i-> this *)
  mutable phi_open_out_tmp: (LF.instantiation * phi) list; (* means this (i -> p *)
  mutable phi_close_in_tmp: (LF.instantiation * phi) list; (* means p )i-> this *)
  mutable phi_close_out_tmp: (LF.instantiation * phi) list; (* means this )i -> p *)

  (* Actual edges of the phi node.
   * These should model actual program control flow
   *)
  mutable phi_next: phi list; (* successors *)
  mutable phi_prev: phi list; (* predecessors *)
  mutable phi_open_in: (LF.instantiation * phi) list; (* means p (i-> this *)
  mutable phi_open_out: (LF.instantiation * phi) list; (* means this (i -> p *)
  mutable phi_close_in: (LF.instantiation * phi) list; (* means p )i-> this *)
  mutable phi_close_out: (LF.instantiation * phi) list; (* means this )i -> p *)

  (* optional integer used to enforce postorder on worklist *)
  mutable phi_postorder_id: int;
}

type phi_kind =
  | PhiVar
  | PhiForked
  | PhiPacked
  | PhiNewlock of LF.lock
  | PhiAcquire of LF.lock
  | PhiRelease of LF.lock
  | PhiDelete of LF.lock

  | PhiSplitCall of LF.lock_effect * phi
      (* The phi that corresponds to calling a function.
       * Parameters:
       * 1. The lock effect of the function that is called
       * 2. The return phi (will always have kind PhiSplitReturn)
       *    that corresponds to this call phi
       *)

  | PhiSplitReturn of LF.lock_effect * phi
      (* The phi that corresponds to the return point of a function call
       * Parameters:
       * 1. The lock effect of the called function
       * 2. The phi right before the corresponding PhiSplitCall node that
       *    corresponds to this phi
       *)

module Phi : HashedOrderedType with type t = phi =
  struct
    type t = phi
    let compare (x: t) (y: t) : int =
      Pervasives.compare x.phi_id y.phi_id
    let equal (x: t) (y: t) : bool =
      (Pervasives.compare x.phi_id y.phi_id = 0)
    let hash (x: t) : int = x.phi_id
  end
module PhiSet = Set.Make(Phi)
(* a hashtable from phi to 'a.
 * it is faster than (phi, 'a) Hashtbl.t,
 * because it uses phi_id to compare
 *)
module PhiHT = Hashtbl.Make(Phi)

module type PhiWorklist =
  sig
    type t
    exception Empty
    val create : unit -> t
    val clear : t -> unit
    val push : phi -> t -> unit
    val pop : t -> phi
    val is_empty : t -> bool
    val length : t -> int
  end


type phiSet = PhiSet.t

(* all phi nodes in the control flow graph *)
let all_phi : phi list ref = ref []

(* "empty" program point.
 * Auxiliary phi used to type effect-less expressions
 *)
let empty_phi : phi = {
  phi_id = 0;
  phi_func = None;
  phi_loc = Cil.locUnknown;
  phi_name = "empty";
  phi_global = false;
  phi_next = [];
  phi_prev = [];
  phi_open_in = [];
  phi_open_out = [];
  phi_close_in = [];
  phi_close_out = [];
  phi_times_visited = ref 0;
  phi_unified = None;
  phi_next_tmp = [];
  phi_prev_tmp = [];
  phi_open_in_tmp = [];
  phi_open_out_tmp = [];
  phi_close_in_tmp = [];
  phi_close_out_tmp = [];
  phi_postorder_id = max_int;
}

let phi_kind_hash : phi_kind PhiHT.t = PhiHT.create 100

let get_phi_kind phi = PhiHT.find phi_kind_hash phi

(* looks up the kind of p in \tenv *)
let set_phi_kind (p: phi) (k: phi_kind) : unit = begin
  assert(not (PhiHT.mem phi_kind_hash p));
  PhiHT.replace phi_kind_hash p k;
end

(******************
 * pretty printing
 ******************)
(* phi2string *)
let dotstring_of_phi (phi: phi) : string =
  "phi#" ^ (string_of_int phi.phi_id) ^ "\\n" ^
  (phi.phi_name^"\\n") ^
  (Pretty.sprint 80 (Cil.d_loc () phi.phi_loc))

let d_phi () (p: phi) : doc =
  if !debug then dprintf "%s#%d at %a" p.phi_name p.phi_id Cil.d_loc p.phi_loc
  else dprintf "%s at %a" p.phi_name Cil.d_loc p.phi_loc

(* phiSet formatting *)
let d_phiset () (ps: phiSet) : doc =
  let f : phi -> doc -> doc = fun x d ->
    d ++ (if d <> nil then line else nil) ++ d_phi () x
  in
  align ++ (PhiSet.fold f ps nil) ++ unalign


(*********************
 * graph construction
 *********************)

(* a counter used to generate unique phi_ids *)
let phi_no : int ref = ref 0

(* creates a fresh phi node, corresponding to !currentLoc program point,
 * with kind k
 *)
let make_phi (s: string) : phi =
  incr phi_no;
  let p = {
    phi_id = !phi_no;
    phi_func = (match !Cil.currentGlobal with Cil.GFun (f, _) -> Some f | _ -> None);
    phi_loc = !Cil.currentLoc;
    phi_name = s;
    phi_global = false;
    phi_next = [];
    phi_prev = [];
    phi_open_in = [];
    phi_open_out = [];
    phi_close_in = [];
    phi_close_out = [];
    phi_times_visited = ref 0;
    phi_unified = None;
    phi_next_tmp = [];
    phi_prev_tmp = [];
    phi_open_in_tmp = [];
    phi_open_out_tmp = [];
    phi_close_in_tmp = [];
    phi_close_out_tmp = [];
    phi_postorder_id = max_int;
  } in
  all_phi := p::!all_phi;
  p

let function_of_phi phi = phi.phi_func
let location_of_phi phi = phi.phi_loc

let starting_phis : phi list ref = ref []

(* creates a control flow edge from phi1 to phi2
 * fails if any of the two is the empty_phi
 *)
let phi_flows (phi1: phi) (phi2: phi) : unit = begin
  if phi1 == phi2 then ()
  else if Phi.compare phi1 empty_phi = 0 then ()
  else if Phi.compare phi2 empty_phi = 0
  then raise (ControlFlowBug "no phi can flow to the empty phi")
  else begin
    if !debug then ignore(E.log "%a flows to %a\n" d_phi phi1 d_phi phi2);
    phi1.phi_next <- phi2::phi1.phi_next;
    phi2.phi_prev <- phi1::phi2.phi_prev;
  end
end

(* for every i, contains the renaming from abstract to concrete phis *)
(*let phi_renaming_map : (phi*phi) list LF.IH.t = LF.IH.create 10*)

(* create an instantiation edge (renaming) instantiating phi_abs to phi_inst
 * at instantiation i
 *)
let inst_phi (phi_abs: phi)
             (phi_inst: phi)
             (polarity: bool)
             (i: LF.instantiation) : unit =
(*  let maplist =
    try
      LF.IH.find phi_renaming_map i
    with
      Not_found -> []
  in
  LF.IH.replace phi_renaming_map i ((phi_abs,phi_inst)::maplist);
*)
  if !debug then ignore(E.log "instantiate %a to %a on %a: %s\n" d_phi phi_abs d_phi phi_inst LF.d_instantiation i (string_of_bool polarity));
  if polarity then begin
    phi_abs.phi_close_out <- (i,phi_inst)::phi_abs.phi_close_out;
    phi_inst.phi_close_in <- (i,phi_abs)::phi_inst.phi_close_in;
  end
  else begin
    phi_abs.phi_open_in <- (i,phi_inst)::phi_abs.phi_open_in;
    phi_inst.phi_open_out <- (i,phi_abs)::phi_inst.phi_open_out;
  end
  

(* Marks a phi point as global, unless it is in the set qs.
 * Global phis are the ones anottating global function pointers, for example.
 *)
let set_global_phi (p: phi) (qs: phiSet) : unit =
  if not (PhiSet.mem p qs) then (
    if !debug then ignore(E.log "set global %a\n" d_phi p);
    p.phi_global <- true
  )

(* unify two phi program points.  equivalent to each flowing to the other *)
let unify_phi (x: phi) (y: phi) : unit =
  if x == y then ()
  else begin
    if !debug then ignore(E.log "unify %a with %a\n" d_phi x d_phi y);
    phi_flows x y;
    phi_flows y x
  end

(* return a new phi to which both p1 and p2 flow *)
let join_phi (p1: phi) (p2: phi) : phi =
  if p1 == p2 then p1
  (*else if Phi.equal p1 empty_phi then p2
  else if Phi.equal p2 empty_phi then p1*)
  else begin
    let np = make_phi "" in
    phi_flows p1 np;
    phi_flows p2 np;
    if !debug then ignore(E.log "join %a with %a => %a\n" d_phi p1 d_phi p2 d_phi np);
    np
  end


(*****************
 * graph printing
 *****************)

(* start set is the set of nodes to print even if they have no edges *)
let print_graph (outf: out_channel) (f: phi -> bool) : unit = begin
  let ps = ref PhiSet.empty in
  let print_phi_edges (outf: out_channel) (p: phi) : unit = begin
    List.iter
      (fun p' -> 
        ps := PhiSet.add p (PhiSet.add p' !ps);
        Printf.fprintf outf "\"%s\"->\"%s\";\n"
          (dotstring_of_phi p) (dotstring_of_phi p'))
      p.phi_next;
    List.iter
      (fun (i,p') -> 
        ps := PhiSet.add p (PhiSet.add p' !ps);
        Printf.fprintf outf "\"%s\"->\"%s\" [label=\"(%s\"];\n"
          (dotstring_of_phi p) (dotstring_of_phi p')
          (LF.dotstring_of_inst i))
      p.phi_open_out;
    List.iter
      (fun (i,p') -> 
        ps := PhiSet.add p (PhiSet.add p' !ps);
        Printf.fprintf outf "\"%s\"->\"%s\" [label=\")%s\"];\n"
          (dotstring_of_phi p) (dotstring_of_phi p')
          (LF.dotstring_of_inst i))
      p.phi_close_out;
  end
  in
  List.iter
    (fun p ->
      print_phi_edges outf p;
      if f p then ps := PhiSet.add p !ps;
    ) !all_phi;
  PhiSet.iter
    (fun p ->
      Printf.fprintf outf "\"%s\" [shape=\"diamond\"];\n"
        (dotstring_of_phi p);
      if p.phi_global then
        Printf.fprintf outf "\"%s\" [peripheries=2];\n"
          (dotstring_of_phi p))
    !ps
end

(****************************
 * union-find, graph slicing
 ****************************)

let reset_graph () = begin
  let sort l =
    List.sort
      (fun p1 p2 ->
        if p1.phi_postorder_id < p2.phi_postorder_id then 1
        else if p1.phi_postorder_id > p2.phi_postorder_id then -1
        else Pervasives.compare p1.phi_id p2.phi_id)
      l
    in
  let sorti l =
    List.sort
      (fun (_,p1) (_,p2) ->
        if p1.phi_postorder_id < p2.phi_postorder_id then 1
        else if p1.phi_postorder_id > p2.phi_postorder_id then -1
        else Pervasives.compare p1.phi_id p2.phi_id)
      l
    in
  let reset p = begin
    p.phi_unified <- None;
    p.phi_next_tmp <- sort p.phi_next;
    p.phi_prev_tmp <- sort p.phi_prev;
    p.phi_open_in_tmp <- sorti p.phi_open_in;
    p.phi_open_out_tmp <- sorti p.phi_open_out;
    p.phi_close_in_tmp <- sorti p.phi_close_in;
    p.phi_close_out_tmp <- sorti p.phi_close_out;
  end
  in
  List.iter reset !all_phi
end

(* union-find on phis *)
let rec findphi p =
  match p.phi_unified with
    None -> p
  | Some p' ->
      let p'' = findphi p' in
      if not (p'' == p') then p.phi_unified <- Some p'';
      p''

(* Unify 2 phi nodes for this analysis (until reset): p1 goes to p2, p2 remains.
 * That shouldn't matter, apply this only to nonrelevant phis
 *)
let unionphi p1 p2 = begin
  let p1' = findphi p1 in
  let p2' = findphi p2 in
  if p1' <> p2' then begin
    let f1 n =
      let n = List.map findphi n in  (* find real names *)
      let n = List.filter (fun p -> (Phi.compare p p1') <> 0) n in (* remove self *)
      List.filter (fun p -> (Phi.compare p p2') <> 0) n (* remove other self :) *)
    in
    let f2 n = List.map (fun (i,p) -> (i,findphi p)) n  (* find real names *)
      (* we don't remove self edges because self instantiation is not vacuously true like self flow *)
    in
    p2'.phi_next_tmp <- f1 (p1'.phi_next_tmp @ p2'.phi_next_tmp);
    p2'.phi_prev_tmp <- f1 (p1'.phi_prev_tmp @ p2'.phi_prev_tmp);
    p2'.phi_open_in_tmp <- f2 (p1'.phi_open_in_tmp @ p2'.phi_open_in_tmp);
    p2'.phi_open_out_tmp <- f2 (p1'.phi_open_out_tmp @ p2'.phi_open_out_tmp);
    p2'.phi_close_in_tmp <- f2 (p1'.phi_close_in_tmp @ p2'.phi_close_in_tmp);
    p2'.phi_close_out_tmp <- f2 (p1'.phi_close_out_tmp @ p2'.phi_close_out_tmp);

    p1'.phi_next_tmp <- [];
    p2'.phi_prev_tmp <- [];
    p2'.phi_open_in_tmp <- [];
    p2'.phi_open_out_tmp <- [];
    p2'.phi_close_in_tmp <- [];
    p2'.phi_close_out_tmp <- [];
    p2'.phi_unified <- Some p1';
  end
end


(*******************************************
 * utilities for traversal of the phi graph 
 *******************************************)

(* return all the "next" phis starting from this.
 * The returned list will include all next, open parenthesis
 * and close parenthesis edges.
 *)
let get_all_children (p: phi) : phi list =
  (List.fold_left
    (fun acc (_,p) -> p::acc)
    (List.fold_left
      (fun acc (_,p) -> p::acc)
      p.phi_next
      p.phi_open_out_tmp)
    p.phi_close_out_tmp)

(* Applies f on every phi in the graph, visiting all nodes reachable from
 * starting_phis in post-order traversal (starting_phis are visited last)
 *)
let postorder_visit (f: phi -> unit) : unit =
  let visited : phiSet ref = ref PhiSet.empty in
  let rec visit (p: phi) : unit =
    if PhiSet.mem p !visited then ()
    else begin
      visited := PhiSet.add p !visited;
      let k =
        try get_phi_kind p 
        with Not_found -> PhiVar
      in
      let _ = match k with
        | PhiSplitCall(_,phi_ret) ->
            visit phi_ret
        | _ -> ()
      in
      List.iter
        (fun p -> visit p)
        (get_all_children p);
      f p
    end
  in
  List.iter visit !starting_phis

let postorder_sort () : unit =
  let next = ref (!phi_no+1) in
  postorder_visit
    (fun p ->
      p.phi_postorder_id <- !next;
      decr next)

(************
 * worklists
 ************)

module UniqueWorklist : functor (W: PhiWorklist) -> PhiWorklist =
  functor (W: PhiWorklist) ->
  struct
    type t = {
      h: unit PhiHT.t; (* membership hash *)
      w: W.t; (* actual worklist *)
    }
    exception Empty = W.Empty

    let create () : t = {
      h = PhiHT.create 100; (* !phi_no; *)
      w = W.create ();
    }

    let clear w =
      PhiHT.clear w.h;
      W.clear w.w

    let push (p: phi) (worklist: t) : unit =
      if PhiHT.mem worklist.h p then ()
      else W.push p worklist.w

    let pop (worklist: t) : phi =
      let p = W.pop worklist.w in
      PhiHT.remove worklist.h p;
      p
    let is_empty w = W.is_empty w.w
    let length w = W.length w.w
  end

module PostorderWorklist : PhiWorklist with type t = phi Heap.t =
  struct
    type t = phi Heap.t
    exception Empty
    let create () : t =
      Heap.create 1000 (*!phi_no*)

    let clear = Heap.clear
    let push (p: phi) (worklist: phi Heap.t) : unit =
      Heap.insert worklist p.phi_postorder_id p

    let pop (worklist: t) : phi =
      if Heap.is_empty worklist then raise Empty
      else snd (Heap.extract_max worklist)
    let is_empty = Heap.is_empty
    let length = Heap.length
  end

module PostorderPhiSet = Set.Make(Phi)

module PostorderSetWorklist : PhiWorklist =
  struct
    type t = PostorderPhiSet.t ref
    exception Empty
    let create () : t = ref PostorderPhiSet.empty

    let clear w =
      w := PostorderPhiSet.empty

    let push (p: phi) (w: t) : unit =
      w := PostorderPhiSet.add p !w

    let pop (w: t) : phi =
      if PostorderPhiSet.is_empty !w then raise Empty
      else
        let p = PostorderPhiSet.max_elt !w in
        w := PostorderPhiSet.remove p !w;
        p

    let is_empty w = PostorderPhiSet.is_empty !w
    
    let length w = PostorderPhiSet.cardinal !w
  end

module StackWorklist : PhiWorklist with type t = phi list ref =
 struct
    type t = phi list ref
    exception Empty
    let create () = ref []
    let clear s = s := []
    let is_empty s = !s = []
    let push p s = s := p::!s
    let pop s =
      match !s with
        [] -> raise Empty
      | h::tl -> s := tl; h
    let length w = List.length !w
  end

module DoubleStackWorklist : PhiWorklist =
 struct
    type t = {
      mutable s1: phi list;
      mutable s2: phi list;
    }
    exception Empty
    let create () = { s1 = []; s2 = []; }
    let clear s = s.s1 <- []; s.s2 <- []
    let is_empty s = (s.s1 = [] && s.s2 = [])
    let push p s = s.s2 <- p::s.s2
    let pop s =
      if is_empty s then raise Empty else
      match s.s1 with
        [] -> (* swap *)
          let p = List.hd s.s2 in
          s.s1 <-
            (*List.sort (fun p1 p2 ->
              if p1.phi_postorder_id < p2.phi_postorder_id then 1
              else if p1.phi_postorder_id > p2.phi_postorder_id then -1
              else Pervasives.compare p1.phi_id p2.phi_id)*)
            (List.tl s.s2);
          s.s2 <- [];
          p
      | h::tl ->
          s.s1 <- tl;
          h
    let length w = (List.length w.s1) + (List.length w.s2)
  end

module QueueWorklist : PhiWorklist with type t = phi Queue.t =
 struct
    type t = phi Queue.t
    exception Empty=Queue.Empty
    let create = Queue.create
    let clear = Queue.clear
    let is_empty = Queue.is_empty
    let push = Queue.push
    let pop = Queue.pop
    let length = Queue.length
  end

(* this one is not a worklist.  It's just a boolean that stores whether something has changed.
 * THIS IS ONLY TO BE USED WITH --only-postorder.
 *)
module BoolWorklist : PhiWorklist =
  struct
    type t = bool ref
    exception Empty
    let create () = ref false
    let clear w = w := false
    let is_empty w = not !w
    let push p w = w := true
    let pop w = raise Empty
    let length w = if !w then 1 else 0
  end

(* backwards worklist *)
module BW = DoubleStackWorklist
module BackwardsWorklist = BW

(* forwards worklist *)
module FW = DoubleStackWorklist
module ForwardsWorklist = FW

(************************
 * Control flow analyses
 ************************)

(* The signature for transfer function of the forwards analysis.
 * The user of the forwards analysis should define a module of this
 * signature, and apply MakeForwardsAnalysis to it, to construct
 * a ForwardsAnalysis that uses the transfer function.
 *)
module type ForwardsTransfer =
  sig
    type state
    val state_before_phi : state PhiHT.t
    val transfer_fwd : phi -> FW.t -> state -> state option
    val starting_state : phi -> state option
    val merge_state : state -> state -> state
    val equal_state : state -> state -> bool
    val translate_state_in : state -> Labelflow.instantiation -> state
    val translate_state_out : state -> Labelflow.instantiation -> state
    val check_state : phi -> state -> unit
    val pretty : unit -> state -> doc
  end

module type ForwardsAnalysis =
  sig
    type state
    (* argument is initial worklist *)
    val solve : phi list -> unit
  end

module MakeForwardsAnalysis =
  functor (A: ForwardsTransfer) ->
  struct
    type state = A.state

    let worklist : FW.t = FW.create ()

    let get_or_create_state (p: phi) : state option =
      try
        Some (PhiHT.find A.state_before_phi p)
      with Not_found -> A.starting_state p

    let set_or_merge_state (p: phi) (s: state) : unit = begin
      let s' = 
        try
        PhiHT.find A.state_before_phi p
        with Not_found -> begin
          FW.push p worklist;
          s
        end
      in
      let s'' = A.merge_state s s' in
      if !debug then ignore(E.log "  old state before %a: %a\n"
                           d_phi p A.pretty s');
      if !debug then ignore(E.log "  incoming state before %a: %a\n"
                           d_phi p A.pretty s);
      if !debug then ignore(E.log "  new state before %a: %a\n"
                           d_phi p A.pretty s'');
      PhiHT.replace A.state_before_phi p s'';
      if not (A.equal_state s' s'') then FW.push p worklist
    end

    let propagate_through (p: phi) : unit = begin
      if !debug then ignore(E.log "propagate through %a\n" d_phi p);
      let before_state = get_or_create_state p in
      if isSome before_state then begin
        let before_state = getSome before_state in
        if !debug then ignore(E.log "  state before: %a\n" A.pretty before_state);
        let after_state = A.transfer_fwd p worklist before_state in
        if isSome after_state then begin
          let after_state = getSome after_state in
          if !debug then ignore(E.log "  state after: %a\n" A.pretty after_state);
          if !debug then ignore(E.log "  d edges to:\n");
          List.iter
            (fun p' -> set_or_merge_state p' after_state)
            p.phi_next_tmp;
          if !debug then ignore(E.log "  open edges to:\n");
          List.iter
            (fun (i,p') ->
              let s = A.translate_state_in after_state i in
              set_or_merge_state p' s)
            p.phi_open_out_tmp;
          if !debug then ignore(E.log "  close edges to:\n");
          List.iter
            (fun (i,p') ->
              let s = A.translate_state_out after_state i in
              set_or_merge_state p' s)
            p.phi_close_out_tmp
        end
      end (* else FW.push p worklist*)
    end

    let solve start_list : unit = begin
      FW.clear worklist;
      reset_graph ();
      if !debug then ignore(E.log "starting points:\n");
      List.iter
        (fun p ->
          if !debug then ignore(E.log "  %a\n" d_phi p);
          FW.push p worklist)
        start_list;
      let rec loop () : unit =
        propagate_through (FW.pop worklist);
        loop ();
      in
      try
        loop ();
      with FW.Empty -> ();
    end
  end

(* backwards analysis *)

(* transfer function signature, the input for the backwards analysis functor *)
module type BackwardsTransfer =
  sig
    type state
    val state_after_phi : state PhiHT.t
    val transfer_back : phi -> BW.t -> state -> state
    val is_relevant : phi -> bool
    val starting_state : phi -> state
    val merge_state : state -> state -> state
    val equal_state : state -> state -> bool
    val translate_state_in : state -> Labelflow.instantiation -> state
    val translate_state_out : state -> Labelflow.instantiation -> state
    val check_state : phi -> state -> unit
    val pretty : unit -> state -> doc
  end

module type BackwardsAnalysis =
  sig
    type state
    val solve : phi list -> unit
  end

module MakeBackwardsAnalysis =
  functor (A: BackwardsTransfer) ->
  struct
    type state = A.state

    let worklist : BW.t = BW.create ()

    let get_or_create_state (p: phi) : state =
      try
        PhiHT.find A.state_after_phi p
      with Not_found -> A.starting_state p

    let set_or_merge_state (p: phi) (s: state) : unit = begin
      let p = findphi p in
      (*ignore(E.log "merging before %a\n" d_phi p);*)
      let s' = 
        try
        PhiHT.find A.state_after_phi p
        with Not_found ->
          (*if not !do_postorder then begin*)
            (*ignore(E.log "pushing1 %a on worklist\n" d_phi p);*)
            BW.push p worklist;
          (*end;*)
          s
      in
      let s'' = A.merge_state s s' in
      if !debug then begin
        ignore(E.log "  old state after %a: %a\n" d_phi p A.pretty s');
        ignore(E.log "  input state after %a: %a\n" d_phi p A.pretty s);
        ignore(E.log "  new state after %a: %a\n" d_phi p A.pretty s'');
      end;
      PhiHT.replace A.state_after_phi p s'';
      if not (A.equal_state s' s'') then begin
        (*ignore(E.log "pushing2 %a on worklist\n" d_phi p);*)
        BW.push p worklist
      end
    end

    let propagate_through (p: phi) : unit = begin
      let p = findphi p in
      incr p.phi_times_visited;
      if !debug then ignore(E.log "propagate through %a\n" d_phi p);
      let after_state = get_or_create_state p in
      let before_state = A.transfer_back p worklist after_state in
      if !debug then ignore(E.log "  input state: %a\n" A.pretty before_state);
      if !debug then ignore(E.log "sub\n");
      (* merge this with each predecessor's state *)
      List.iter
        (fun p' ->
          let p' = findphi p' in
          set_or_merge_state p' before_state)
          p.phi_prev_tmp;

      if !debug then ignore(E.log "in\n");
      (* translate along all open_i edges that end on p *) 
      List.iter
        (fun (i,p') ->
          if !debug then ignore(E.log " %a:\n" LF.d_instantiation i);
          let p' = findphi p' in
          let s = A.translate_state_out before_state i in
          set_or_merge_state p' s)
        p.phi_open_in_tmp;

      if !debug then ignore(E.log "out\n");
      (* translate along all close_i edges that end on p *) 
      List.iter
        (fun (i,p') ->
          let p' = findphi p' in
          let s = A.translate_state_in after_state i in
          set_or_merge_state p' s)
        p.phi_close_in_tmp;
    end

    let remove_irrelevant phi : unit = begin
      if A.is_relevant phi then () else begin
        let f = (fun p -> if A.is_relevant p then () else unionphi phi p) in
        List.iter f phi.phi_next_tmp;
        List.iter f phi.phi_prev_tmp;
      end
    end

    let solve start_list : unit = begin
      BW.clear worklist;
      if !do_slice_phi then List.iter remove_irrelevant !all_phi;
      postorder_sort ();
      reset_graph ();
      let postorder_visits = ref 0 in
      List.iter (fun p -> BW.push p worklist) start_list;
      let rec loop () : unit =
        if !do_postorder then begin
          incr postorder_visits;
          if !debug then ignore (E.log "another iteration: %d / %d\n" (BW.length worklist) (!phi_no));
          BW.clear worklist;
          postorder_visit propagate_through;
          if not (BW.is_empty worklist)
          then loop ()
        end else begin
          propagate_through (BW.pop worklist);
          loop();
        end
      in
      (try
        loop ();
      with BW.Empty -> ());
      if !do_postorder && !do_count_postorder then ignore(E.log "postorder visits: %d\n" !postorder_visits);
    end
          
  end

let count_phi_visits (f: phi -> string) : unit =
  List.iter
    (fun p ->
      ignore(E.log "%s : %d %s\n" (dotstring_of_phi p) !(p.phi_times_visited) (f p)))
    !all_phi
