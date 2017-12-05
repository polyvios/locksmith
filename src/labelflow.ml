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
(*open Lockutil*)
open Pretty
module LN = Labelname
module E = Errormsg

let debug = ref false
let flow_effect = ref false (* set by options in shared.ml *)
let no_effects = ref false
let use_untagged = ref false

let options = [
  "--debug-labelflow",
    Arg.Set(debug),
    " Extra output from the labelflow module";

  "--no-continuation-effects",
    Arg.Set(no_effects),
    " Don't allocate any banshee labels for continuation effects";

  "--use-untagged-nodes",
    Arg.Set(use_untagged),
    " Attempt to use untagged nodes for non-fork effects in banshee";
]

exception LabelFlowBug of string

module type CFLT =
  sig
    type node
    type instantiation
    module InstHT : Hashtbl.S with type key = instantiation
    module Node : Lockutil.HashedOrderedType with type t = node
    module NodeSet : Set.S with type elt = node
    module NodeHT : Hashtbl.S with type key = node

    val options : (string * Arg.spec * string) list

    val make_node : string -> bool -> Cil.fundec option -> Cil.location -> bool -> node
    val update_node_location : node -> Cil.location -> node
    val string_of_node : node -> string
    val dotstring_of_node : node -> string
    val fresh_inst : unit -> instantiation
    val make_sub_edge : node -> node -> unit
    val make_open_edge : node -> node -> instantiation -> unit
    val make_close_edge : node -> node -> instantiation -> unit
    val set_global : node -> unit
    val is_global : node -> bool
    val is_concrete : node -> bool
    val string_of_inst : instantiation -> string
    val total_nodes : unit -> int
    val get_all_that_reach_m : node -> (node -> 'a) ->
                               ('a -> 'b -> 'b) -> 'b -> 'b
    val get_all_that_reach_pn : node -> (node -> 'a) ->
                               ('a -> 'b -> 'b) -> 'b -> 'b
    val done_adding : unit -> unit
  end

module CFL : CFLT = (* choose one: *)
  (*Falsecfl*)
  Bansheemlifc
  (*Mycfl*)

module Graph =
  struct
    open CFL
    type edge =
      | SubEdge of node * node
      | OpenEdge of node * node * instantiation
      | CloseEdge of node * node * instantiation

    module Edge =
      struct
        type t = edge
        let compare e1 e2 = Pervasives.compare e1 e2
      end
    module EdgeSet = Set.Make(Edge)

    let edges : EdgeSet.t ref = ref EdgeSet.empty
    let sub_edges : node NodeHT.t = NodeHT.create 100
    let open_edges : (node*instantiation) NodeHT.t = NodeHT.create 100
    let close_edges : (node*instantiation) NodeHT.t = NodeHT.create 100

    let make_sub_edge x y =
      edges := EdgeSet.add (SubEdge(x, y)) !edges;
      NodeHT.add sub_edges x y;
      CFL.make_sub_edge x y

    let make_open_edge x y i =
      edges := EdgeSet.add (OpenEdge(x, y, i)) !edges;
      NodeHT.add open_edges x (y,i);
      CFL.make_open_edge x y i

    let make_close_edge x y i =
      edges := EdgeSet.add (CloseEdge(x, y, i)) !edges;
      NodeHT.add close_edges x (y,i);
      CFL.make_close_edge x y i

    let make_inst_edge n1 n2 p i =
      if p then make_close_edge n1 n2 i
      else make_open_edge n2 n1 i

    let print_graph outf : NodeSet.t = begin
      let ns = ref NodeSet.empty in
      let dotstring_of_edge (e: edge) : string =
        match e with
        | SubEdge(n1, n2) ->
            ns := NodeSet.add n1 (NodeSet.add n2 !ns);
            ("\"" ^ (dotstring_of_node n1)
            ^ "\" -> \"" ^ (dotstring_of_node n2) ^ "\";\n")
        | OpenEdge(n1, n2, i) ->
            ns := NodeSet.add n1 (NodeSet.add n2 !ns);
            ("\"" ^ (dotstring_of_node n1)
            ^ "\" -> \"" ^ (dotstring_of_node n2)
            ^ "\" [label=\"(" ^ (string_of_inst i) ^ "\"];\n")
        | CloseEdge(n1, n2, i) ->
            ns := NodeSet.add n1 (NodeSet.add n2 !ns);
            ("\"" ^ (dotstring_of_node n1)
            ^ "\" -> \"" ^ (dotstring_of_node n2)
            ^ "\" [label=\")" ^ (string_of_inst i) ^ "\"];\n")
      in

      EdgeSet.iter 
        (fun e -> Printf.fprintf outf "%s" (dotstring_of_edge e))
        !edges;

      NodeSet.iter
        (fun n ->
          if CFL.is_concrete n then
            Printf.fprintf outf "\"%s\" [shape=\"box\"];\n" (dotstring_of_node n);
          if CFL.is_global n then
            Printf.fprintf outf "\"%s\" [peripheries=2]\n" (dotstring_of_node n);
        )
        !ns;
      !ns
    end

    let shortest_path n1 n2 =
      let find_all ht n =
        try NodeHT.find_all ht n
        with Not_found -> [] in
      let visited = NodeHT.create 10000 in
      let q : (node * edge list) Queue.t = Queue.create() in
      let rec bfs () =
        if Queue.is_empty q then raise Not_found;
        let n,path = Queue.pop q in
        if Node.equal n n2 then List.rev path
        else if NodeHT.mem visited n then bfs()
        else (
          NodeHT.add visited n ();
          List.iter
            (fun n' ->
              if not (NodeHT.mem visited n') then
                Queue.push (n', SubEdge(n,n')::path) q)
            (find_all sub_edges n);
          List.iter
            (fun (n',i) ->
              if not (NodeHT.mem visited n') then
                Queue.push (n', OpenEdge(n,n',i)::path) q)
            (find_all open_edges n);
          List.iter
            (fun (n',i) ->
              if not (NodeHT.mem visited n') then
              Queue.push (n', CloseEdge(n,n',i)::path) q)
            (find_all close_edges n);
          bfs()
        )
      in
      Queue.push (n1,[]) q;
      bfs ()

  end

type node = CFL.node

type instantiation = {
  inst_id: CFL.instantiation;
  inst_loc: Cil.location;
  inst_is_pack: bool; (* true for exists, false for forall *)
  inst_fun_name: string;

  (*inst_lock_effect: effect;*)
  (* pairs of instantiated labels.
   * (abstract, instance) for normal, (instance, abstract) for _r reversed
   *)
  inst_locks: (lock, lock) Hashtbl.t;
  inst_locks_r: (lock, lock) Hashtbl.t;
  inst_rho: (rho, rho) Hashtbl.t;
  inst_rho_r: (rho, rho) Hashtbl.t;
}

and lock = {
  lock_cfl_node       : node;
  (** the banshee node that corresponds to this lock. *)

  lock_label_name     : LN.label_name;
  (** special struct used to print better error messages *)

  lock_path           : instantiation list;
  (** the "through" path, i.e.  which instantiations this lock has
   * gone through
   *)

  mutable lock_linear : bool;  (** true if the lock is linear *)
}

and rho = {
  rho_cfl_node   : node;
  (** the banshee node that corresponds to this location. *)

  rho_label_name : LN.label_name;
  (** special struct used to print better error messages *)
}

module Rho : Lockutil.HashedOrderedType with type t = rho =
  struct
    type t = rho
    let compare (x: t) (y: t) : int =
      CFL.Node.compare x.rho_cfl_node y.rho_cfl_node
    let equal (x: t) (y: t) : bool =
      (CFL.Node.compare x.rho_cfl_node y.rho_cfl_node = 0)
    let hash (x: t) : int = CFL.Node.hash x.rho_cfl_node
  end
module RhoHT = Hashtbl.Make(Rho)
module RhoSet = Set.Make(Rho)
type rhoSet = RhoSet.t

type rhopath = Graph.edge list

module Lock =
  struct
    type t = lock
    let compare (x: t) (y: t) : int =
      CFL.Node.compare x.lock_cfl_node y.lock_cfl_node
    let equal (x: t) (y: t) : bool = 
      (CFL.Node.compare x.lock_cfl_node y.lock_cfl_node = 0)
    let hash (x: t) : int = CFL.Node.hash x.lock_cfl_node
  end
module LockHT = Hashtbl.Make(Lock)
module LockSet = Set.Make(Lock)
type lockSet = LockSet.t

type effect = {
  effect_read_node: node;
  effect_write_node: node;
  effect_share_node: node;
}

type chi = {
  chi_read_node: node;
  chi_write_node: node;
}

type lock_effect = lock

module Effect : Set.OrderedType with type t = effect =
  struct
    type t = effect
    let compare (x: t) (y: t) : int =
      CFL.Node.compare x.effect_read_node y.effect_read_node
  end
module EffectSet = Set.Make(Effect)
module EffectMap = Map.Make(Effect)
type effectSet = EffectSet.t

let inst_equal (i1: instantiation) (i2: instantiation) : bool =
  (i1.inst_id = i2.inst_id)

module Inst =
  struct
    type t = instantiation
    let equal : t -> t -> bool = inst_equal
    let hash (i: t) = Hashtbl.hash (i.inst_id)
  end
module InstHT = Hashtbl.Make(Inst)

module NodeHT = CFL.NodeHT

(* instantiations -- map from CFL.instantiation to instantiation *)
let all_inst : instantiation CFL.InstHT.t = CFL.InstHT.create 1000

(* all rhos -- map from node to rho *)
let all_rho = NodeHT.create 1000

(* all rhos that correspond to a malloc or & *)
let all_concrete_rho: rhoSet ref = ref RhoSet.empty

(* all locks -- map from node to lock *)
let all_locks = NodeHT.create 50

(* all locks that correspond to newlock *)
let all_concrete_locks : lockSet ref = ref LockSet.empty

(* all effect nodes *)
let all_effects : effect list ref = ref []

(******************
 * pretty printing
 ******************)

(* dot-string. format in a way that's best for "dot" graph drawing *)
let dotstring_of_inst i = CFL.string_of_inst i.inst_id
let dotstring_of_lock lock = CFL.dotstring_of_node lock.lock_cfl_node
let dotstring_of_rho rho = CFL.dotstring_of_node rho.rho_cfl_node
let dotstring_of_lock_effect e = CFL.dotstring_of_node e.lock_cfl_node
let dotstring_of_write_effect e = CFL.dotstring_of_node e.effect_write_node
let dotstring_of_read_effect e = CFL.dotstring_of_node e.effect_read_node

(* instantiation formatting. use string_of_inst if you just want the index *)
let d_instantiation () (i: instantiation) : doc =
    text (if i.inst_is_pack then "pack " else (i.inst_fun_name^" "))
      ++ Cil.d_loc () i.inst_loc

(* lock *)
let d_lock () lock =
  text (if lock.lock_linear then "" else "non linear ") ++
  text (if CFL.is_concrete lock.lock_cfl_node then "concrete " else "") ++
  (List.fold_left
    (fun c i -> c ++ text " -> " ++ d_instantiation () i)
    (LN.d_label_name () lock.lock_label_name ++
     text (CFL.string_of_node lock.lock_cfl_node))
    lock.lock_path)


(* lockSet formatting *)
let d_lockset () (l: lockSet) : doc = 
  if LockSet.is_empty l then text "<empty>" else
  let slist = LockSet.fold
    (fun x d -> let s = sprint 80 (d_lock () x) in s::d)
    l [] in
  let slist = List.sort Pervasives.compare slist in
  align ++ List.fold_left (fun d s -> d ++ (if d = nil then nil else line) ++ text s) nil slist ++
  unalign

(* rho *)
let d_rho () rho =
  dprintf "%a%s" LN.d_label_name rho.rho_label_name
    (if !debug then (CFL.dotstring_of_node rho.rho_cfl_node)
    else (CFL.string_of_node rho.rho_cfl_node))

let d_rhopath () (fromrho, torho: rho * rho) : doc =
  let path =
    try Graph.shortest_path fromrho.rho_cfl_node torho.rho_cfl_node 
    with Not_found -> []
  in
  let rec d_rp p =
    match p with
      [] -> nil
    | Graph.SubEdge(_, n2)::tl -> 
        let r2 = NodeHT.find all_rho n2 in
        (dprintf " => %a\n" d_rho r2) ++ d_rp tl
    | Graph.OpenEdge(_, n2, i)::tl ->
        let r2 = NodeHT.find all_rho n2 in
        let i' = CFL.InstHT.find all_inst i in
        (dprintf " => %a at %a\n" d_rho r2 d_instantiation i') ++ d_rp tl
    | Graph.CloseEdge(_, n2, i)::tl ->
        let r2 = NodeHT.find all_rho n2 in
        let i' = CFL.InstHT.find all_inst i in
        (dprintf " => %a at %a\n" d_rho r2 d_instantiation i') ++ d_rp tl
  in
  d_rho () fromrho ++ align ++ 
    (if path = [] then line else d_rp path)
    ++ unalign

(* rhoSet formatting *)
let d_rhoset () (rs: rhoSet) : doc =
  if RhoSet.is_empty rs then text "<empty>" else
  let slist = RhoSet.fold
    (fun x d ->
      let t = sprint 80 (d_rho () x) in
      t::d)
    rs []
  in
  let slist = List.sort Pervasives.compare slist in
  align ++ List.fold_left (fun d s -> d ++ (if d = nil then nil else line) ++ text s) nil slist ++
    (*RhoSet.fold
      (fun x d -> d ++ (if d <> nil then line else nil) ++ d_rho () x)
      rs
      nil ++*)
  unalign

(* effect formatting *)
let d_effect () (e: effect) : doc =
  text "(" ++ align ++
    text ((CFL.dotstring_of_node e.effect_read_node) ^ ",\n") ++
    text ((CFL.dotstring_of_node e.effect_write_node) ^ ",\n") ++
    text ((CFL.dotstring_of_node e.effect_share_node) ^ ",\n") ++
    (*text ((CFL.dotstring_of_node e.effect_lock_node)) ++*)
    unalign ++ text ")"

(* effectSet formatting *)
let d_effectset () (es: effectSet) : doc =
  let f : effect -> doc -> doc = fun x d ->
    d ++ (if d <> nil then line else nil) ++ d_effect () x
  in
  align ++ (EffectSet.fold f es nil) ++ unalign

let d_chi () (x: chi) : doc =
  text ("X-"^(CFL.dotstring_of_node x.chi_read_node))

(*********************
 * graph construction
 *********************)

let done_adding = CFL.done_adding

(* create a simple CFL node. it's marked as concrete if concrete=true.
 * the new node is tagged by "name"
 *)
let make_node (name: string) (concrete: bool) (tagged: bool) : node =
  let f = match !Cil.currentGlobal with Cil.GFun (f, _) -> Some f | _ -> None in
  let l = !Cil.currentLoc in
  let n = CFL.make_node name concrete f l tagged in
  (*ignore(E.log "creating %s\n" (CFL.string_of_node n));*)
  n


(* instantiations *)

(* Creates an instantiation marker.
 * Instantiations are marked with an effect variable.
 * Only the "lock" effect of the variable is used in order
 * to skip "translating" irrelevant locks through that instantiation
 *)
let make_instantiation (is_pack: bool) (instantiated: string) : instantiation =
  let i = {
    inst_id = CFL.fresh_inst ();
    inst_is_pack = is_pack;
    inst_fun_name = instantiated;
    inst_loc = !Cil.currentLoc;
    inst_locks = Hashtbl.create 1;
    inst_locks_r = Hashtbl.create 1;
    (*inst_lock_effect = e;*)
    inst_rho = Hashtbl.create 10;
    inst_rho_r = Hashtbl.create 10;
  } in
  CFL.InstHT.add all_inst i.inst_id i;
  i


(* rhos *)

(* a simple counter of rhos generated so far *)
let rho_no = ref 0

(* creates a fresh rho label labeled "s".
 * If concrete=true, the fresh rho is marked as "constant", i.e. it
 * corresponds to a malloc() or "&" memory allocation
 *)
let make_rho (n: LN.label_name) (concrete: bool) : rho =
  incr rho_no;
  let r = {
    rho_cfl_node = make_node "" concrete true;
    rho_label_name = n;
  } in
  if concrete then all_concrete_rho := RhoSet.add r !all_concrete_rho;
  NodeHT.add all_rho r.rho_cfl_node r;
  r

let update_rho_location rho loc =
  ignore (CFL.update_node_location rho.rho_cfl_node loc); rho

let count_rho () = !rho_no

(* mark r as global, unless it is in the set qs *)
let set_global_rho (r: rho) : unit =
  CFL.set_global r.rho_cfl_node

(* A special constant used to denote a lock with no name
 * in the current context.  It is the result of any substitution that's
 * not defined.
 *)
let unknown_rho : rho = make_rho (LN.Const "unknown") false

(* create a subtyping edge from r1 to r2 *)
let rho_flows r1 r2 = begin
  if Rho.equal r1 unknown_rho then ()
  else if Rho.equal r2 unknown_rho then ()
  else Graph.make_sub_edge r1.rho_cfl_node r2.rho_cfl_node
end

(* creates a unification constraint between x and y.
 * is equivalent with two subtyping constraints
 *)
let unify_rho (x: rho) (y: rho) : unit =
  if Rho.equal x y then ()
  else begin
    rho_flows x y;
    rho_flows y x;
  end

(* returns the join of two rhos.
 * a fresh rho to which they both flow
 *)
(*let join_rho (r1: rho) (r2: rho) : rho =
  if r1 = r2 then r1
  else if r1 == unknown_rho then r2
  else if r2 == unknown_rho then r1
  else begin
    let nr = make_rho "r" false in
    rho_flows r1 nr;
    rho_flows r2 nr;
    nr
  end
  *)

(* instantiate rabs to rinst with polarity "polarity" at instantiation i *)
let inst_rho (rabs: rho)
             (rinst: rho)
             (polarity: bool)
             (i: instantiation)
             : unit = begin
  (try
    let r = Hashtbl.find i.inst_rho rabs in
    unify_rho r rinst
  with Not_found -> begin
    Hashtbl.add i.inst_rho rabs rinst;
    Hashtbl.add i.inst_rho_r rinst rabs;
  end);
  Graph.make_inst_edge rabs.rho_cfl_node rinst.rho_cfl_node polarity i.inst_id
end


(* locks *)

(* a counter of locks *)
let lock_no = ref 0

(* creates a fresh lock.
 * If concrete = true, the lock is marked as concrete, i.e. corresponds to
 * a call to newlock.
 *)
let make_lock (n: LN.label_name) (concrete: bool) : lock = begin
  incr lock_no;
  let l = {
    lock_cfl_node = make_node "" concrete true;
    lock_label_name = n;
    lock_linear = true;
    lock_path = [];
  } in
  if concrete then all_concrete_locks := LockSet.add l !all_concrete_locks;
  NodeHT.add all_locks (l.lock_cfl_node) l;
  l
end

(* unknown lock.  used to denote locks not visible in the current context *)
let unknown_lock : lock = make_lock (LN.Const "unknown") false

let lock_flows l1 l2 = begin
  if Lock.equal l1 unknown_lock then ()
  else if Lock.equal l2 unknown_lock then ()
  else Graph.make_sub_edge l1.lock_cfl_node l2.lock_cfl_node
end

(* empty lock.  should never include any concrete lock in its solution *)
let empty_lock : lock = make_lock (LN.Const "empty") false

(* mark r as global (add all selfloops *)
let set_global_lock (r: lock) : unit =
  CFL.set_global r.lock_cfl_node

(* instantiate (with both polarities) labs linst at instantiation i *)
let inst_lock labs linst i = begin
  Hashtbl.add i.inst_locks labs linst;
  Hashtbl.add i.inst_locks_r linst labs;
  Graph.make_inst_edge labs.lock_cfl_node linst.lock_cfl_node true i.inst_id;
  Graph.make_inst_edge labs.lock_cfl_node linst.lock_cfl_node false i.inst_id;
end

(* unify two lock variables *)
let unify_locks (l1: lock) (l2: lock) : unit =
  if Lock.equal l1 l2 then ()
  else (
    Graph.make_sub_edge l1.lock_cfl_node l2.lock_cfl_node;
    Graph.make_sub_edge l2.lock_cfl_node l1.lock_cfl_node;
  )

(* mark a lock as non-linear *)
let set_nonlinear (l: lock) : unit =
  l.lock_linear <- false

let is_concrete_lock (l: lock) : bool =
  CFL.is_concrete l.lock_cfl_node

module CH = Hashtbl.Make(
  struct
    type t = (lock * instantiation)
    let equal (l1,i1 : t) (l2,i2 : t) : bool =
      (CFL.Node.compare l1.lock_cfl_node l2.lock_cfl_node = 0)
      && (i1.inst_id = i2.inst_id)
    let hash (l,i : t) = 2 * (CFL.Node.hash l.lock_cfl_node) + Hashtbl.hash i.inst_id
  end)

let clone_map = CH.create 10

let clone_lock (l: lock) (i: instantiation) =
  assert(CFL.is_concrete l.lock_cfl_node);
  try
    CH.find clone_map (l,i)
  with Not_found -> begin
    if not l.lock_linear then begin
      inst_lock l l i;
      l
    end
    else if List.mem i l.lock_path then begin
      (*set_nonlinear l;*)
      l
    end
    else begin
      let ll = {
        lock_cfl_node = make_node "" true true;
        lock_label_name = l.lock_label_name;
        lock_linear = true;
        lock_path = i::l.lock_path;
      } in
      inst_lock l ll i;
      all_concrete_locks := LockSet.add ll !all_concrete_locks;
      NodeHT.add all_locks (ll.lock_cfl_node) ll;
      CH.add clone_map (l,i) ll;
      ll
    end
  end

let is_nonlinear (l: lock) : bool =
  l.lock_linear

let is_global_lock (l: lock) : bool =
  CFL.is_global l.lock_cfl_node

let is_global_rho (r: rho) : bool =
  CFL.is_global r.rho_cfl_node

(* effects *)

let effect_no = ref 0

let make_share_effect_node =
  incr effect_no;
  let dummye = make_node "Es_dummy" false true in
  function (s:string) ->
    if !flow_effect then begin
      incr effect_no;
      make_node ("Es_"^s) false true
    end
    else
      dummye

let dummy_node = make_node "DUMMY" false true

(* create an effect variable \varepsilon.  it captures the read and write
 * effects and the locks that are "touched".
 *)
let make_effect (s: string) (tagged: bool) : effect = begin
  incr effect_no;
  incr effect_no;
  let tag =
    if !use_untagged then tagged else true in
  let e = {
    effect_read_node = if !no_effects then dummy_node else make_node ("Er_"^s) false tag;
    effect_write_node = if !no_effects then dummy_node else make_node ("Ew_"^s) false tag;
    effect_share_node = if !no_effects then dummy_node else make_share_effect_node s;
    (*effect_lock_node = make_node ("El_"^s) false;*)
  } in
  all_effects := e::!all_effects;
  e
end

(* empty effect.  used to type effect-less expressions. Bug will be raised
 * if something is effected in it
 *)
let empty_effect : effect = make_effect "empty" false


let chi_no = ref 0
let all_chi = ref []
let make_chi (s: string) : chi =
  incr chi_no;
  let e = {
    chi_read_node = make_node ("Xr_"^s) false true;
    chi_write_node = make_node ("Xw_"^s) false true;
  } in
  all_chi := e::!all_chi;
  e

let inst_chi xabs xinst polarity i = begin
  Graph.make_inst_edge xabs.chi_read_node xinst.chi_read_node polarity i.inst_id;
  Graph.make_inst_edge xabs.chi_write_node xinst.chi_write_node polarity i.inst_id;
end

let add_to_read_chi r x =
  Graph.make_sub_edge r.rho_cfl_node x.chi_read_node
let add_to_write_chi r x =
  Graph.make_sub_edge r.rho_cfl_node x.chi_write_node
let set_global_chi x =
  CFL.set_global x.chi_read_node;
  CFL.set_global x.chi_write_node
let chi_flows x1 x2 : unit =
  if x1 == x2 then () else begin
    Graph.make_sub_edge x1.chi_read_node x2.chi_read_node;
    Graph.make_sub_edge x1.chi_write_node x2.chi_write_node;
  end

let make_lock_effect () = make_lock (LN.Const "lock effect") false

let set_global_lock_effect = set_global_lock
let inst_lock_effect labs linst polarity i = begin
  Graph.make_inst_edge labs.lock_cfl_node linst.lock_cfl_node polarity i.inst_id;
end
let lock_effect_flows = lock_flows

(* marks global effect (e.g. effect of global function ptr),
 * unless e is in the set qs
 *)
let set_global_effect (e: effect) (qs: effectSet) : unit =
  if not (EffectSet.mem e qs) then begin
    CFL.set_global e.effect_read_node;
    CFL.set_global e.effect_write_node;
    if !flow_effect then CFL.set_global e.effect_share_node;
    (*CFL.set_global e.effect_lock_node;*)
  end

(* create a subtyping edge from e1 to e2.  For r/w effects
 * e2 corresponds to a program point BEFORE e1. (effects go backwards).
 * or: the solution of e2 includes e1.  Sharing effects are
 * standard.
 *)
let effect_flows e1 e2 = 
  if Effect.compare e1 e2 = 0 then () else begin
    Graph.make_sub_edge (e1.effect_read_node) (e2.effect_read_node);
    Graph.make_sub_edge (e1.effect_write_node) (e2.effect_write_node);
    if !flow_effect then
      Graph.make_sub_edge (e2.effect_share_node) (e1.effect_share_node);
    (*Graph.make_sub_edge (e1.effect_lock_node) (e2.effect_lock_node);*)
  end

let chi_in_effect c e = begin
  Graph.make_sub_edge (c.chi_read_node) (e.effect_read_node);
  Graph.make_sub_edge (c.chi_write_node) (e.effect_write_node);
end

(* instantiation effect eabs to einst with polarity "polarity" at
 * instantiation site i
 *)
let inst_effect eabs einst polarity i = begin
  Graph.make_inst_edge eabs.effect_read_node einst.effect_read_node polarity i.inst_id;
  Graph.make_inst_edge eabs.effect_write_node einst.effect_write_node polarity i.inst_id;
  if !flow_effect then
    Graph.make_inst_edge eabs.effect_share_node einst.effect_share_node (not polarity) i.inst_id;
  (*Graph.make_inst_edge eabs.effect_lock_node einst.effect_lock_node polarity i.inst_id;*)
end

(* create an "effect-membership" edge: loc is read in ef *)
let add_to_read_effect (loc: rho) (ef: effect) : unit =
  if Effect.compare ef empty_effect = 0
  then () else
    (*raise (LabelFlowBug "cannot add variables to the empty effect");*)
  (* UPDATE: reading in the empty effect is ok.  we need it *)
  if Rho.compare loc unknown_rho = 0 then ()
  else Graph.make_sub_edge loc.rho_cfl_node ef.effect_read_node

(* create an "effect-membership" edge: loc is written in ef *)
let add_to_write_effect (loc: rho) (ef: effect) : unit =
  if Effect.compare ef empty_effect = 0
  then raise (LabelFlowBug "cannot add variables to the empty effect");
  if Rho.compare loc unknown_rho = 0 then ()
  else Graph.make_sub_edge loc.rho_cfl_node ef.effect_write_node

(* create an "effect-membership" edge: loc is written in ef *)
let add_to_share_effect (loc: rho) (ef: effect) : unit =
  if Effect.compare ef empty_effect = 0
  then raise (LabelFlowBug "cannot add variables to the empty sharing effect");
  if Rho.compare loc unknown_rho = 0 then ()
  else Graph.make_sub_edge loc.rho_cfl_node ef.effect_share_node
      (* note: don't check !flow_effect here; won't get called unless
	 !flow_effect is turned on in the first place *)

(* create an "effect-membership" edge: l is created, acquired,
 * released or destroyed in ef
 *)
let add_to_lock_effect (l: lock) (ef: lock_effect) : unit = begin
  assert (l <> unknown_lock);
  assert (ef <> unknown_lock);
  Graph.make_sub_edge l.lock_cfl_node ef.lock_cfl_node
end

(* return the join of two effects
 * this works backwards, the result flows to both inputs
 *)
let join_effects (e1: effect) (e2: effect) : effect =
  if Effect.compare e1 e2 = 0 then e1
  else begin
    let eff = make_effect "join" false in
    effect_flows eff e1;
    effect_flows eff e2;
    eff
  end

(* unify two effect variables *)
let unify_effects (e1: effect) (e2: effect) : unit =
  if Effect.compare e1 e2 = 0 then ()
  else begin
    effect_flows e2 e1;
    effect_flows e1 e2;
  end


(***********
 * solution
 ***********)

(* simple query *)
let is_pack (i: instantiation) : bool = i.inst_is_pack

(* solution of lock *)
let get_lock_p2set (l: lock) : lockSet = begin
    let lockset = CFL.get_all_that_reach_m
      l.lock_cfl_node
      (NodeHT.find all_locks)
      LockSet.add
      LockSet.empty
    in
    lockset
end

(* solution of rho *)
let get_rho_p2set_m (r: rho) : rhoSet = begin
  let rhoset = CFL.get_all_that_reach_m
      r.rho_cfl_node
      (NodeHT.find all_rho)
      RhoSet.add
      RhoSet.empty
  in
  (*ignore(E.log "rho-p2set: %d\n" (RhoSet.cardinal rhoset));*)
  rhoset
end

let get_rho_p2set_pn (r: rho) : rhoSet = begin
  let rhoset = CFL.get_all_that_reach_pn
      r.rho_cfl_node
      (NodeHT.find all_rho)
      RhoSet.add
      RhoSet.empty
  in
  rhoset
end

(* set closure wrt "flows to". unknowns are removed from the solution *)
let close_lockset (ls: lockSet) : lockSet = begin
  let f (l: lock) (s: lockSet) =
    if LockSet.mem l s then s
    else LockSet.union s (get_lock_p2set l) in
  let locks = LockSet.fold f ls LockSet.empty in
  let result = LockSet.remove unknown_lock locks in
  result
(*  LockSet.remove unknown_lock (LockSet.fold f ls LockSet.empty)*)
end

let close_rhoset_m (rs: rhoSet) : rhoSet = begin
  let f (r:rho) (s: rhoSet) =
    if RhoSet.mem r s then s
    else RhoSet.union s (get_rho_p2set_m r) in
  let rhos = RhoSet.fold f rs RhoSet.empty in
  let result = RhoSet.remove unknown_rho rhos in
  result
end

let close_rhoset_pn (rs: rhoSet) : rhoSet = begin
  let f (r:rho) (s: rhoSet) = RhoSet.union s (get_rho_p2set_pn r) in
  let rhos = RhoSet.fold f rs RhoSet.empty in
  let result = RhoSet.remove unknown_rho rhos in
  result
end

let concrete_rhoset (rs: rhoSet) : rhoSet =
  RhoSet.filter (fun r -> CFL.is_concrete r.rho_cfl_node) rs

let concrete_lockset (ls: lockSet) : lockSet =
  LockSet.filter
    (fun l -> l.lock_linear && (CFL.is_concrete l.lock_cfl_node))
    ls

(* return the read,write solution of an effect variable *)
let solve_rw_effect_pn (e: effect) : (rhoSet * rhoSet) = begin
  if !debug then ignore(E.log "solving r/w effect:");
  let solv n =
    CFL.get_all_that_reach_pn
      n
      (fun x -> try NodeHT.find all_rho x with Not_found -> unknown_rho)
      RhoSet.add
      RhoSet.empty
  in
  let x = solv e.effect_read_node in
  let y = solv e.effect_write_node in
  if !debug then ignore(E.log "\n");
  (x,y)
end

(*
let solve_rw_effect_m (e: effect) : (rhoSet * rhoSet) = begin
  if !debug then ignore(E.log "solving r/w effect (matched):");
  let solv n =
    CFL.get_all_that_reach_m
      n
      (fun x -> try NodeHT.find all_rho x with Not_found -> unknown_rho)
      RhoSet.add
      RhoSet.empty
  in
  let x = solv e.effect_read_node in
  let y = solv e.effect_write_node in
  if !debug then ignore(E.log "\n");
  (x,y)
end
*)

let solve_share_effect_pn (e: effect) : rhoSet = begin
  if !debug then ignore(E.log "solving sharing effect:");
      (* note: don't check !flow_effect here; won't get called unless
	 !flow_effect is turned on in the first place *)
  let solv n =
    CFL.get_all_that_reach_pn
      n
      (fun x -> try NodeHT.find all_rho x with Not_found -> unknown_rho)
      RhoSet.add
      RhoSet.empty
  in
  let x = solv e.effect_share_node in
  if !debug then ignore(E.log "\n");
  x
end

let solve_chi_m (x: chi) : (rhoSet * rhoSet) = begin
  if !debug then ignore(E.log "solving chi:");
  let solv n =
    CFL.get_all_that_reach_m
      n
      (fun x -> try NodeHT.find all_rho x with Not_found -> unknown_rho)
      RhoSet.add
      RhoSet.empty
  in
  let read = RhoSet.remove unknown_rho (solv x.chi_read_node) in
  let write = RhoSet.remove unknown_rho (solv x.chi_write_node) in
  if !debug then ignore(E.log "\n");
  (read, write)
end
let solve_chi_pn (x: chi) : (rhoSet * rhoSet) = begin
  if !debug then ignore(E.log "solving chi:");
  let solv n =
    CFL.get_all_that_reach_pn
      n
      (fun x -> try NodeHT.find all_rho x with Not_found -> unknown_rho)
      RhoSet.add
      RhoSet.empty
  in
  let r = solv x.chi_read_node in
  let w = solv x.chi_write_node in
  if !debug then ignore(E.log "\n");
  (r,w)
end

let dump_all_chi () =
  if !debug then ignore(E.log "dump chi\n");
  List.iter
    (fun x ->
      ignore(E.log "%a :\n" d_chi x);
      let r,w = solve_chi_m x in
      ignore(E.log "  read: %a\n" d_rhoset r);
      ignore(E.log "  write: %a\n" d_rhoset w)
    )
    !all_chi

(* translate a lock positively through the given instantiation
 * (outwards, abstract becomes instance)
 *)
let translate_lock i l =
  if not l.lock_linear then unknown_lock else
  try
    Hashtbl.find i.inst_locks l
  with
  | Not_found ->
      if CFL.is_global l.lock_cfl_node then l
      else unknown_lock

(* translate a lockset positively through the given instantiation *)
let translate_lockset_out (i:instantiation) (ls: lockSet) : lockSet =
  (*
  let result = 
  *)
  LockSet.fold
    (fun x y -> LockSet.add (translate_lock i x) y)
    ls
    LockSet.empty
  (*
  in
  ignore(E.log "abstract lockset   : %a\n" d_lockset ls);
  ignore(E.log "translated lockset : %a\n" d_lockset result);
  result
  *)

(* translate a lockset negatively through the given instantiation
 * (inwards, instance becomes abstract)
 *) 
let translate_lock_r i l =
  if not l.lock_linear then unknown_lock else
    try
      Hashtbl.find i.inst_locks_r l
    with Not_found ->
      if CFL.is_global l.lock_cfl_node then l else unknown_lock
  (*else unknown_lock*)

(* translate a lockset negatively through the given instantiation *)
let translate_lockset_in (i:instantiation) (ls: lockSet) : lockSet =
let r =
  LockSet.fold
    (fun x y -> LockSet.add (translate_lock_r i x) y)
    ls
    LockSet.empty
in
(*ignore(E.log "translate lockset from %a to %a\n" d_lockset ls d_lockset r);*)
r

(* translate a rho positively through the given instantiation
 * (outwards, abstract becomes instance
 *)
let translate_rho_out (i:instantiation) (r: rho) =
  try
    Hashtbl.find i.inst_rho r
  with Not_found ->
    if i.inst_is_pack then unknown_rho else
    if CFL.is_global r.rho_cfl_node then r
    else if CFL.is_concrete r.rho_cfl_node then begin
      (*
      let r' = make_rho "r" true in
      inst_rho r r' true i;
      inst_rho r r' false i;
      r'
      *)
      r
    end
    else unknown_rho

(* translate a rho negatively through the given instantiation
 * (inwards, instance becomes abstract
 *)
let translate_rho_in (i: instantiation) (r: rho) =
  try
    Hashtbl.find i.inst_rho_r r
  with Not_found ->
    if i.inst_is_pack then unknown_rho else
    if CFL.is_global r.rho_cfl_node then r
    else if CFL.is_concrete r.rho_cfl_node then r
    else unknown_rho

(* translate a rhoSet positively (outwards) through the given instantiation *)
let translate_rhoset_out (i: instantiation) (rs: rhoSet) : rhoSet =
  RhoSet.fold
    (fun x y -> RhoSet.add (translate_rho_out i x) y)
    rs
    RhoSet.empty

(* translate a rhoSet positively (outwards) through the given instantiation *)
let translate_rhoset_in (i: instantiation) (rs: rhoSet) : rhoSet =
  RhoSet.fold
    (fun x y -> RhoSet.add (translate_rho_in i x) y)
    rs
    RhoSet.empty

let solve_lock_effect_m (e: lock_effect) : lockSet = begin
  if !debug then ignore(E.log "solving lock effect:");
  let solv n =
    CFL.get_all_that_reach_m
      n
      (fun x -> try NodeHT.find all_locks x with Not_found -> unknown_lock)
      LockSet.add
      LockSet.empty
  in
  solv e.lock_cfl_node
end

let split_lockset (ls: lockSet) (eff: lock_effect) =
  let eff_solution = solve_lock_effect_m eff in
  let reaching = LockSet.inter eff_solution ls in
  let nonreaching = LockSet.diff ls reaching in
  (reaching, nonreaching)
  (*LockSet.fold
    (fun l (reaching, nonreaching) ->
      if CFL.reaches_m l.lock_cfl_node eff.lock_cfl_node
      then LockSet.add l reaching, nonreaching
      else reaching, LockSet.add l nonreaching)
    ls
    (LockSet.empty, LockSet.empty)
    *)

let inst_iter (f: instantiation -> unit) : unit =
  CFL.InstHT.iter (fun _ -> f) all_inst

let concrete_rho_iter (f: rho -> unit) : unit =
  RhoSet.iter f (!all_concrete_rho)

let concrete_lock_iter (f: lock -> unit) : unit =
  LockSet.iter f !all_concrete_locks

(* compute all non-linear lock names *)
let close_nonlinear () : unit = begin
  concrete_lock_iter
    (fun l ->
      if not l.lock_linear then
        let ls = get_lock_p2set l in
        let cs = concrete_lockset ls in
        LockSet.iter set_nonlinear cs
    );
end



(***************
 * graph output
 ***************)

let print_graph (outf: out_channel) : unit = begin
  let ns = Graph.print_graph outf in
  CFL.NodeSet.iter
    (fun n ->
      try
        let rho = NodeHT.find all_rho n in
        Printf.fprintf outf "\"%s\" [label=\"%s\\n%s\"]\n"
          (CFL.dotstring_of_node rho.rho_cfl_node)
          (LN.string_of_label_name rho.rho_label_name)
          (CFL.dotstring_of_node rho.rho_cfl_node)
      with Not_found -> ())
    ns;
  CFL.NodeSet.iter
    (fun n ->
      try
        let lock = NodeHT.find all_locks n in
        Printf.fprintf outf "\"%s\" [label=\"%s\\n%s\"]\n"
          (CFL.dotstring_of_node lock.lock_cfl_node)
          (LN.string_of_label_name lock.lock_label_name)
          (CFL.dotstring_of_node lock.lock_cfl_node)
      with Not_found -> ())
    ns;
end

let get_stats () : string = begin
  (* locks, locations, effects, chi, total *)
  Printf.sprintf "%d %d %d %d %d" !lock_no !rho_no (!effect_no) (!chi_no * 2) (CFL.total_nodes())
end

