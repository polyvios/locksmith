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
open Printf

module E = Errormsg
module Q = Queue

let debug = ref false

let options = [
  "--debug-mycfl",
  Arg.Set(debug),
  " Debugging info about our CFL implementation."
]

type location = Cil.location
let string_of_loc l = Pretty.sprint 80 (Cil.d_loc () l)

type instantiation = int
let inst_compare = (-)
module InstHT = Inthash

module rec Node :
  sig
    type t = {
      node_id: int;
      mutable node_name       : string;
      node_function           : Cil.fundec option;
      mutable node_location   : location;
      mutable node_global     : bool;
      node_concrete           : bool;
      mutable node_succ_s     : NodeSet.t;
      mutable node_pred_s     : NodeSet.t;
      mutable node_succ_p_m_p : NodeSet.t;
      mutable node_pred_p_m_p : NodeSet.t;
      mutable node_succ_p_p_p : NodeSet.t;
      mutable node_pred_p_p_p : NodeSet.t;
      mutable node_succ_n_m_n : NodeSet.t;
      mutable node_pred_n_m_n : NodeSet.t;
      mutable node_succ_n_n_n : NodeSet.t;
      mutable node_pred_n_n_n : NodeSet.t;
      mutable node_succ_k_m_i : NodeSet.t InstHT.t;
      mutable node_pred_k_m_i : NodeSet.t InstHT.t;
      mutable node_succ_m_i_k : NodeSet.t InstHT.t;
      mutable node_pred_m_i_k : NodeSet.t InstHT.t;
      mutable node_succ_m_m_m : NodeSet.t;
      mutable node_pred_m_m_m : NodeSet.t;
      mutable node_succ_m_n_g : NodeSet.t;
      mutable node_pred_m_n_g : NodeSet.t;
      mutable node_succ_m_g_p : NodeSet.t;
      mutable node_pred_m_g_p : NodeSet.t;
    }
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int
  end
  =
  struct
    type t = {
      node_id: int;
      mutable node_name       : string;
      node_function           : Cil.fundec option;
      mutable node_location   : location;
      mutable node_global     : bool;
      node_concrete           : bool;
      mutable node_succ_s     : NodeSet.t;
      mutable node_pred_s     : NodeSet.t;
      mutable node_succ_p_m_p : NodeSet.t;
      mutable node_pred_p_m_p : NodeSet.t;
      mutable node_succ_p_p_p : NodeSet.t;
      mutable node_pred_p_p_p : NodeSet.t;
      mutable node_succ_n_m_n : NodeSet.t;
      mutable node_pred_n_m_n : NodeSet.t;
      mutable node_succ_n_n_n : NodeSet.t;
      mutable node_pred_n_n_n : NodeSet.t;
      mutable node_succ_k_m_i : NodeSet.t InstHT.t;
      mutable node_pred_k_m_i : NodeSet.t InstHT.t;
      mutable node_succ_m_i_k : NodeSet.t InstHT.t;
      mutable node_pred_m_i_k : NodeSet.t InstHT.t;
      mutable node_succ_m_m_m : NodeSet.t;
      mutable node_pred_m_m_m : NodeSet.t;
      mutable node_succ_m_n_g : NodeSet.t;
      mutable node_pred_m_n_g : NodeSet.t;
      mutable node_succ_m_g_p : NodeSet.t;
      mutable node_pred_m_g_p : NodeSet.t;
    }

    let compare x y = x.node_id - y.node_id

    let hash x = x.node_id (* unique *)

    let equal n1 n2 =
      n1.node_id = n2.node_id

  end
and NodeSet : Set.S with type elt = Node.t = Set.Make(Node)
module NodeHT = Hashtbl.Make(Node)

type nodeset = NodeSet.t
type node = Node.t

let is_concrete n = n.Node.node_concrete
let is_global n = n.Node.node_global

let node_index = ref 1
let total_nodes () = !node_index
let nodes = ref NodeSet.empty
let make_node (name: string) (concrete: bool) (func: Cil.fundec option) (loc: location) (_: bool): node = begin
  let n = {
    Node.node_id         = !node_index;
    Node.node_name       = name;
    Node.node_concrete   = concrete;
    Node.node_global     = false; (* set this to true using set_global *)
    Node.node_location   = loc;
    Node.node_function   = func;
    Node.node_succ_s     = NodeSet.empty;
    Node.node_pred_s     = NodeSet.empty;
    Node.node_succ_p_m_p = NodeSet.empty;
    Node.node_pred_p_m_p = NodeSet.empty;
    Node.node_succ_p_p_p = NodeSet.empty;
    Node.node_pred_p_p_p = NodeSet.empty;
    Node.node_succ_n_m_n = NodeSet.empty;
    Node.node_pred_n_m_n = NodeSet.empty;
    Node.node_succ_n_n_n = NodeSet.empty;
    Node.node_pred_n_n_n = NodeSet.empty;
    Node.node_succ_k_m_i = InstHT.create 10;
    Node.node_pred_k_m_i = InstHT.create 10;
    Node.node_succ_m_i_k = InstHT.create 10;
    Node.node_pred_m_i_k = InstHT.create 10;
    Node.node_succ_m_m_m = NodeSet.empty;
    Node.node_pred_m_m_m = NodeSet.empty;
    Node.node_succ_m_n_g = NodeSet.empty;
    Node.node_pred_m_n_g = NodeSet.empty;
    Node.node_succ_m_g_p = NodeSet.empty;
    Node.node_pred_m_g_p = NodeSet.empty;
  } in
  incr node_index;
  nodes := NodeSet.add n !nodes;
  n
end
let update_node_location node loc = node.Node.node_location <- loc; node
let dotstring_of_node n = n.Node.node_name ^"#"^ (string_of_int (Node.hash n)) ^ "\\n" ^ (Pretty.sprint 80 (Cil.d_loc () n.Node.node_location))
let string_of_node n = n.Node.node_name ^ ":" ^ (Pretty.sprint 80 (Cil.d_loc () n.Node.node_location))


type edge_kind =
  | Edged
  | Edgeopeni of instantiation
  | Edgeclosei of instantiation
  | EdgeS
  | EdgeP
  | EdgeN
  | EdgeM
  | Edgeg
  | Edgee
  | EdgeK of instantiation

type edge = {
  edge_kind : edge_kind;
  edge_source : node;
  edge_target : node;
}

let knd2str = function
  | EdgeS -> "S"
  | EdgeP -> "P"
  | EdgeN -> "N"
  | EdgeM -> "M"
  | Edgeg -> "g"
  | Edgee -> "e"
  | EdgeK(i) -> "K" ^ (string_of_int i)
  | Edgeopeni(i) -> "(" ^ (string_of_int i)
  | Edgeclosei(i) -> ")" ^ (string_of_int i)
  | Edged -> "d"

module EdgeSet = Set.Make(
  struct
    type t = edge
    let compare x y =
      let sources = Node.compare x.edge_source y.edge_source in
      if sources <> 0 then sources else
      let targets = Node.compare x.edge_target y.edge_target in
      if targets <> 0 then targets else
      Pervasives.compare x.edge_kind y.edge_kind
  end
)

type edgeset = EdgeSet.t

let is_terminal e : bool =
  match e.edge_kind with
  | EdgeS
  | EdgeP
  | EdgeN
  | EdgeM
  | EdgeK(_) -> false
  | Edgeg
  | Edgee
  | Edgeopeni(_)
  | Edgeclosei(_)
  | Edged -> true

let inst_index : int ref = ref 0
let fresh_inst () : int =
  incr inst_index;
  !inst_index

let insts = ref []
let edges = ref EdgeSet.empty
let globals = ref NodeSet.empty

let make_instantiation () : instantiation =
  let id = fresh_inst() in
  insts := id::!insts;
  id

let string_of_inst = string_of_int

let string_of_edge e = 
  knd2str e.edge_kind
  ^ " from " ^ (string_of_loc e.edge_source.Node.node_location)
  ^ " to " ^ (string_of_loc e.edge_target.Node.node_location)

let print_edge e = begin
  print_string (knd2str e.edge_kind);
  print_string (" from " ^ (string_of_loc e.edge_source.Node.node_location));
  print_string (" to " ^ (string_of_loc e.edge_target.Node.node_location));
  (*print_newline();*)
end

let make_edge (knd: edge_kind) (src: node) (tgt: node) (*glob: bool*) = begin
  let e = {
    edge_kind = knd;
    edge_source = src;
    edge_target = tgt;
  } in
  if not (EdgeSet.mem e !edges) then begin
    edges := EdgeSet.add e !edges;
  end;
  e
end

let solved = ref false

let make_sub_edge (src: node) (tgt: node) : unit =
  assert (not !solved);
  ignore(make_edge Edged src tgt)

let make_open_edge (src: node) (tgt: node) (i: instantiation) : unit =
  assert (not !solved);
  ignore(make_edge (Edgeopeni i) src tgt)

let make_close_edge (src: node) (tgt: node) (i: instantiation) : unit =
  assert (not !solved);
  ignore(make_edge (Edgeclosei i) src tgt)

let set_global (n: node) : unit = begin
  assert (not !solved);
  if n.Node.node_global then ()
  else begin
    n.Node.node_global <- true;
    globals := NodeSet.add n !globals;
    ignore(make_edge Edgeg n n);
  end
end

let get_nodeset (i: instantiation) (h: nodeset InstHT.t) =
  try
    InstHT.find h i 
  with
    | Not_found -> NodeSet.empty

let get_all_that_reach_m (n: node)
                         (find: node -> 'a)
                         (add_node: 'a -> 'b -> 'b)
                         (set : 'b)
                         : 'b =
  assert !solved;
  let reachm = EdgeSet.fold
    (fun e acc -> if (e.edge_kind = EdgeM) && (Node.equal e.edge_target n)
      then (
        NodeSet.add e.edge_source acc
        )
      else acc)
    !edges
    NodeSet.empty
  in
  NodeSet.fold
    (fun (x: node) (s: 'b) ->
      add_node (find x) s)
    reachm
    set

let get_all_that_reach_pn (n: node)
                         (find: node -> 'a)
                         (add_node: 'a -> 'b -> 'b)
                         (set : 'b)
                         : 'b =
  assert !solved;
  ignore(E.log "point1. so far so good\n");
  let reachpn : NodeSet.t= EdgeSet.fold
    (fun e ns -> if e.edge_kind = EdgeS && Node.equal e.edge_target n
      then (
        ignore(E.log "really fubar: %s\n" (string_of_node e.edge_source));
        NodeSet.add e.edge_source ns)
      else ns)
    !edges
    NodeSet.empty
  in
  let reachpn2 = n.Node.node_pred_s in
  NodeSet.fold
    (fun (x: node) (s: 'b) -> add_node (find x) s)
    reachpn
    set

let reaches x y =
  assert !solved;
  let e = {
    edge_kind = EdgeS;
    edge_source = x;
    edge_target = y;
  } in
  let b = EdgeSet.mem e !edges in
  let b' = NodeSet.mem x y.Node.node_pred_s in
  (*NodeSet.iter (fun n -> ignore(E.log "%s\n" (string_of_node n))) y.Node.node_pred_s;*)
  assert (b = b');
  b

let reaches_m x y =
  assert !solved;
  let e = {
    edge_kind = EdgeM;
    edge_source = x;
    edge_target = y;
  } in
  let b = EdgeSet.mem e !edges in
  let b' = NodeSet.mem x y.Node.node_pred_m_m_m in
  assert (b = b');
  b

let g: edgeset ref = ref EdgeSet.empty
let doFullCFL () = begin
  ignore(E.log "solving mycfl\n");
  NodeSet.iter (fun x -> begin
      ignore(make_edge EdgeP x x);
      ignore(make_edge EdgeN x x);
      ignore(make_edge EdgeM x x);
    end) !nodes;
  let q = Q.create() in
  EdgeSet.iter (fun x -> Q.add x q) !edges;
  (*fprintf stdout "foobari %d\n" !node_index ;
  fprintf stdout "foobar %d\n" (Q.length q) ;*)
  let rec loopCFL () = begin
      if Q.is_empty q then ()
      else let e = Q.take q in
      (*print_string "processing ";
      print_edge e;
      print_newline();*)
      if not (EdgeSet.mem e !g) then
      begin
        g := EdgeSet.add e !g;
        (*fprintf stdout "e = %s\n" (string_of_edge e) ;*)
        match e.edge_kind with
          | EdgeS ->
                e.edge_target.Node.node_pred_s <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_s;
          | EdgeP ->
              begin
                (*S <- P N*)
                e.edge_target.Node.node_pred_s <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_s;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeS e.edge_source n) q) e.edge_target.Node.node_succ_s;
                (*P <- M P*)
                e.edge_source.Node.node_succ_p_m_p <- NodeSet.add e.edge_target e.edge_source.Node.node_succ_p_m_p;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeP n e.edge_target) q) e.edge_source.Node.node_pred_p_m_p;
                (*P <- p P*)
                e.edge_source.Node.node_succ_p_p_p <- NodeSet.add e.edge_target e.edge_source.Node.node_succ_p_p_p;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeP n e.edge_target) q) e.edge_source.Node.node_pred_p_p_p;
                (* M <- g P *)
                e.edge_source.Node.node_succ_m_g_p <- NodeSet.add e.edge_target e.edge_source.Node.node_succ_m_g_p;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM n e.edge_target) q) e.edge_source.Node.node_pred_m_g_p;
              end
          | EdgeN -> 
              begin
                (*S <- P N*)
                e.edge_source.Node.node_succ_s <- NodeSet.add e.edge_target e.edge_source.Node.node_succ_s;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeS n e.edge_target) q) e.edge_source.Node.node_pred_s;
                (*N <- M N*)
                e.edge_source.Node.node_succ_n_m_n <- NodeSet.add e.edge_target e.edge_source.Node.node_succ_n_m_n;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeN n e.edge_target) q) e.edge_source.Node.node_pred_n_m_n;
                (*N <- n N*)
                e.edge_source.Node.node_succ_n_n_n <- NodeSet.add e.edge_target e.edge_source.Node.node_succ_n_n_n;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeN n e.edge_target) q) e.edge_source.Node.node_pred_n_n_n;
                (* M <- N g *)
                e.edge_target.Node.node_pred_m_n_g <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_m_n_g;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM e.edge_source n) q) e.edge_target.Node.node_succ_m_n_g;
              end
          | EdgeM ->
              (* P <- M P *)
              e.edge_target.Node.node_pred_p_m_p <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_p_m_p;
              NodeSet.iter (fun n -> Q.add (make_edge EdgeP e.edge_source n) q) e.edge_target.Node.node_succ_p_m_p;
              (* N <- M N *)
              e.edge_target.Node.node_pred_n_m_n <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_n_m_n;
              NodeSet.iter (fun n -> Q.add (make_edge EdgeN e.edge_source n) q) e.edge_target.Node.node_succ_n_m_n;
              (* Ki<- M )i*)
              let f i = begin
                let nspred = get_nodeset i e.edge_target.Node.node_pred_k_m_i in
                let nssucc = get_nodeset i e.edge_target.Node.node_succ_k_m_i in
                InstHT.replace e.edge_target.Node.node_pred_k_m_i i (NodeSet.add e.edge_source nspred);
                NodeSet.iter (fun n -> Q.add (make_edge (EdgeK i) e.edge_source n) q) nssucc;
              end
              in List.iter f !insts;
              (* M <- M M *)
              e.edge_target.Node.node_pred_m_m_m <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_m_m_m;
              NodeSet.iter (fun n -> Q.add (make_edge EdgeM e.edge_source n) q) e.edge_target.Node.node_succ_m_m_m;
              e.edge_source.Node.node_succ_m_m_m <- NodeSet.add e.edge_target e.edge_source.Node.node_succ_m_m_m;
              NodeSet.iter (fun n -> Q.add (make_edge EdgeM n e.edge_target) q) e.edge_source.Node.node_pred_m_m_m;
              (* S <- M *)
              Q.add (make_edge EdgeM e.edge_source e.edge_target) q;
          | Edgeg ->
                (* M <- g P *)
                e.edge_target.Node.node_pred_m_g_p <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_m_g_p;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM e.edge_source n) q) e.edge_target.Node.node_succ_m_g_p;
                (* M <- N g *)
                e.edge_source.Node.node_succ_m_n_g <- NodeSet.add e.edge_target e.edge_source.Node.node_succ_m_n_g;
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM n e.edge_target) q) e.edge_source.Node.node_pred_m_n_g;
          | Edgee -> ()
          | EdgeK(i) ->
              begin
                (* M <- (i Ki *)
                let nssucc = get_nodeset i e.edge_source.Node.node_succ_m_i_k in
                let nspred = get_nodeset i e.edge_source.Node.node_pred_m_i_k in
                InstHT.replace e.edge_source.Node.node_succ_m_i_k i (NodeSet.add e.edge_target nssucc);
                NodeSet.iter (fun n -> Q.add (make_edge EdgeM n e.edge_target) q) nspred;
              end
          | Edgeopeni(i) ->
              (* M <- (i Ki *)
              let nssucc = get_nodeset i e.edge_target.Node.node_succ_m_i_k in
              let nspred = get_nodeset i e.edge_target.Node.node_pred_m_i_k in
              InstHT.replace e.edge_target.Node.node_pred_m_i_k i (NodeSet.add e.edge_source nspred);
              NodeSet.iter (fun n -> Q.add (make_edge EdgeM e.edge_source n) q) nssucc;
              (* N <- (i N *)
              e.edge_target.Node.node_pred_n_n_n <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_n_n_n;
              NodeSet.iter (fun n -> Q.add (make_edge EdgeN e.edge_source n) q) e.edge_target.Node.node_succ_n_n_n;
          | Edgeclosei(i) ->
              (* Ki <- M )i *)
              let nssucc = get_nodeset i e.edge_source.Node.node_succ_k_m_i in
              let nspred = get_nodeset i e.edge_source.Node.node_pred_k_m_i in
              InstHT.replace e.edge_source.Node.node_succ_k_m_i i (NodeSet.add e.edge_target nssucc);
              NodeSet.iter (fun n -> Q.add (make_edge (EdgeK i) n e.edge_target) q) nspred;
              (* P <- )i P *)
              e.edge_target.Node.node_pred_p_p_p <- NodeSet.add e.edge_source e.edge_target.Node.node_pred_p_p_p;
              NodeSet.iter (fun n -> Q.add (make_edge EdgeP e.edge_source n) q) e.edge_target.Node.node_succ_p_p_p;
          | Edged ->
              Q.add (make_edge EdgeM e.edge_source e.edge_target) q;
      end;
      loopCFL ()
  end
  in
  loopCFL()
end

(* Given a node n and a find function mapping nodes to targets,
   find all elements that reach n, use find to covert them
   to targets, and then use add_node to union them in to the
   original set, returning the resulting set. *)
let done_adding () : unit =
  assert (not !solved);
  doFullCFL ();
  solved := true

let test () = 
  let d0 = make_node "d0" false None Cil.locUnknown false in
  let d1 = make_node "d1" false None Cil.locUnknown false in
  let d2 = make_node "d2" false None Cil.locUnknown false in
  let d3 = make_node "d3" false None Cil.locUnknown false in
  let d4 = make_node "d4" false None Cil.locUnknown false in
  let d5 = make_node "d5" false None Cil.locUnknown false in
  let d6 = make_node "d6" false None Cil.locUnknown false in
  let d7 = make_node "d7" false None Cil.locUnknown false in
  let d8 = make_node "d8" false None Cil.locUnknown false in
  let d9 = make_node "d9" false None Cil.locUnknown false in
  let i1 = make_instantiation() in
  let i2 = make_instantiation() in
  let i3 = make_instantiation() in
  let i4 = make_instantiation() in
  begin
  ignore(make_sub_edge d1 d2);
  ignore(make_open_edge d0 d1 i1);
  ignore(make_close_edge d2 d3 i1);
  ignore(make_close_edge d2 d4 i2);
  ignore(make_open_edge d5 d1 i2);
  ignore(make_close_edge d7 d2 i3);
  ignore(make_sub_edge d1 d8);
  ignore(make_close_edge d8 d9 i3);
  ignore(make_close_edge d9 d6 i4);
  set_global d8;

  print_string "solving...";
  print_newline();

  done_adding ();

  assert (reaches d1 d1);          print_string "reflexivity ok\n";
  assert (reaches_m d1 d1);        print_string "reflexivity ok\n";
  assert (reaches d1 d2);          print_string "subtype edges ok\n";
  assert (reaches_m d1 d2);        print_string "subtype edges ok\n";
  assert (reaches d0 d3);          print_string "matched paths ok\n";
  assert (reaches_m d0 d3);        print_string "matched paths ok\n";
  assert (reaches d5 d4);          print_string "matched paths 2 ok\n";
  assert (reaches_m d5 d4);        print_string "matched paths 2 ok\n";
  assert (not (reaches d0 d4));    print_string "unmatched paths ok\n";
  assert (not (reaches_m d0 d4));  print_string "unmatched paths ok\n";
  assert (not (reaches d5 d3));    print_string "unmatched paths 2 ok\n";
  assert (not (reaches_m d5 d3));  print_string "unmatched paths 2 ok\n";
  assert (not (reaches d5 d0));    print_string "non-paths ok\n";
  assert (not (reaches_m d5 d0));  print_string "non-paths ok\n";
  assert ((reaches d0 d2));        print_string "pn works\n";
  assert (not (reaches_m d0 d2));  print_string "pn works\n";
  assert (reaches d5 d2);          print_string "n path ok\n";
  assert (not (reaches_m d5 d2));  print_string "n path ok\n";
  assert (reaches d0 d2);          print_string "n path ok\n";
  assert (not (reaches_m d0 d2));  print_string "n path ok\n";
  assert (reaches d1 d3);          print_string "p path ok\n";
  assert (not (reaches_m d1 d3));  print_string "p path ok\n";
  assert (reaches d2 d4);          print_string "p path ok\n";
  assert (not (reaches_m d2 d4));  print_string "p path ok\n";
  assert (reaches d2 d3);          print_string "p path ok\n";
  assert (not (reaches_m d2 d3));  print_string "p path ok\n";
  assert (reaches d7 d3);          print_string "p path ok\n";
  assert (not (reaches_m d7 d3));  print_string "p path ok\n";
  assert (reaches d7 d2);          print_string "p path ok\n";
  assert (not (reaches_m d7 d2));  print_string "p path ok\n";
  assert (reaches d0 d8);          print_string "global ok\n";
  assert (reaches_m d0 d8);        print_string "global ok\n";
  assert (reaches d5 d8);          print_string "global ok\n";
  assert (reaches_m d5 d8);        print_string "global ok\n";
  assert (reaches d0 d9);          print_string "global ok\n";
  assert (reaches_m d0 d9);        print_string "global ok\n";
  assert (reaches d5 d9);          print_string "global ok\n";
  assert (reaches_m d5 d9);        print_string "global ok\n";
  assert (not (reaches d7 d9));    print_string "nonpath ok\n";
  assert (not (reaches_m d7 d9));  print_string "nonpath ok\n";
  assert (reaches_m d1 d6);        print_string "global ok\n";
  assert (reaches_m d1 d6);        print_string "global ok\n";
  assert (reaches_m d8 d6);        print_string "global ok\n";
  assert (not (reaches_m d9 d6));  print_string "global ok\n";

end

(*let _ = test()*)
