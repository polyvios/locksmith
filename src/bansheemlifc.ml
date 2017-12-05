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

module E = Errormsg

type banshee_node
type instantiation = int

module InstHT = Inthash

let string_of_inst = string_of_int

external make_tagged_banshee_node : string -> banshee_node = "banshee_make_tagged_node"
external make_untagged_banshee_node : string -> banshee_node = "banshee_make_untagged_node"
external banshee_make_open_edge : banshee_node -> banshee_node -> int -> unit = "banshee_open_edge"
external banshee_make_close_edge : banshee_node -> banshee_node -> int -> unit = "banshee_close_edge"
external banshee_reaches_pn : banshee_node -> banshee_node -> bool = "banshee_reachespn"
external banshee_reaches_m : banshee_node -> banshee_node -> bool = "banshee_reaches_m"
external banshee_hash : banshee_node -> int = "banshee_hash"
external bansheeInit : bool -> unit = "banshee_initcfl"
external banshee_make_sub_edge : banshee_node -> banshee_node -> unit = "banshee_make_subtype_edge"
external banshee_set_global : banshee_node -> unit = "banshee_mark_node_global"
external banshee_reaches_m_list : banshee_node -> banshee_node list = "banshee_reaches_m_list"
external banshee_reaches_pn_list : banshee_node -> banshee_node list = "banshee_reaches_pn_list"

let do_dump_dyckcfl : bool ref = ref false
let dump_dyckcfl_file : out_channel Lazy.t = lazy (open_out "graph.txt")
let options = []
(*
  "--dump-dyckcfl-trace",
    Arg.Set(do_dump_dyckcfl),
    "Write the trace of calls to banshee in graph.txt so that it can be replayed.";
]
*)

type node = {
  nid : int;
  nnode: banshee_node;
  nname: string;
  nfunc: Cil.fundec option;
  mutable nloc: Cil.location;
  mutable nglob: bool;
  nconcrete: bool;
}

type edge =
  | SubEdge of node * node
  | OpenEdge of node * node * instantiation
  | CloseEdge of node * node * instantiation

module Node =
  struct
    type t = node
    let hash n = n.nid
    let compare n1 n2 =
      let h1 = hash n1 in  (* assumes unique hashing *)
      let h2 = hash n2 in
      if h1 == h2 then 0
      else if h1 > h2 then 1
      else -1
        (* jf -- not safe to subtract; e.g., integer overflow *)
        (* (hash n1) - (hash n2) *)
    let equal n1 n2 =
      let h1 = hash n1 in  (* assumes unique hashing *)
      let h2 = hash n2 in
      h1 == h2
  end
module NodeSet = Set.Make(Node)
module NodeHT = Hashtbl.Make(Node)

module BansheeNode =
  struct
    type t = banshee_node
    let equal n1 n2 =  (* possibly could use ==? *)
      let h1 = banshee_hash n1 in
      let h2 = banshee_hash n2 in
      h1 == h2  (* assumes unique hashing *)
    let hash = banshee_hash (* possibly could use Hashtbl.hash? *)
  end
module BansheeNodeHT = Hashtbl.Make(BansheeNode)

let all_nodes = BansheeNodeHT.create 100

(* after this becomes true, fresh_inst doesn't work *)
let done_inst : bool ref = ref false

let next_inst = ref 0
let fresh_inst () : instantiation =
  assert (not !done_inst);
  incr next_inst; !next_inst
let next_id = ref 1
let make_node (n: string) (c: bool) (f : Cil.fundec option) (l: Cil.location) (tagged: bool) : node = begin
  let nn = {
    nid = !next_id;
    nnode = if tagged
            then make_tagged_banshee_node n
            else make_untagged_banshee_node n;
    nname = n;
    nloc = l;
    nfunc = f;
    nglob = false;
    nconcrete = c;
  } in
  incr next_id;
(* if c then concretes := NodeSet.add nn !concretes; *)
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) "c %d\n" nn.nid;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  BansheeNodeHT.add all_nodes nn.nnode nn;
  nn
end

let update_node_location node loc = node.nloc <- loc; node

let dotstring_of_node n = n.nname ^"#"^ (string_of_int (Node.hash n)) ^ "\\n" ^ (Pretty.sprint 80 (Cil.d_loc () n.nloc))
let string_of_node n = n.nname ^ ":" ^ (Pretty.sprint 80 (Cil.d_loc () n.nloc))

let is_concrete n = n.nconcrete
let set_global n = begin
  n.nglob <- true;
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) "g %d\n" n.nid;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  banshee_set_global n.nnode;
end
let is_global n = n.nglob

let total_nodes () : int = !next_id

let reaches_m x y = begin
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) "m %d %d\n" x.nid y.nid;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  banshee_reaches_m x.nnode y.nnode
end

let reaches_pn x y = begin
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) "p %d %d\n" x.nid y.nid;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  banshee_reaches_pn x.nnode y.nnode
end

(* usually called after typing *)
let done_adding () : unit =
(* presumably not needed any more *)
  assert (not !done_inst);
  done_inst := true;
  ()


(* Given a node n and a find function mapping nodes to targets,
   find all elements that reach n, use find to covert them
   to targets, and then use add_node to union them in to the
   original set, returning the resulting set. *)
let get_all_that_reach_m (n: node)
                         (find: node -> 'a)
                         (add_node: 'a -> 'b -> 'b)
                         (set : 'b)
                         : 'b =
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) "M %d\n" n.nid;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  let nl = banshee_reaches_m_list n.nnode in
  List.fold_left
    (fun (s: 'b) (x: banshee_node) ->
      (
       (*polyvios: this should never throw Not_found, so i'm not catching it*)
       let n = BansheeNodeHT.find all_nodes x in
       let elt = find n in
       add_node elt s
      )
    )
    set nl
let get_all_that_reach_pn (n: node)
                          (find: node -> 'a)
                          (add_node: 'a -> 'b -> 'b)
                          (set : 'b)
                          : 'b =
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) "P %d\n" n.nid;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  let nl = banshee_reaches_pn_list n.nnode in
  List.fold_left
    (fun (s: 'b) (x: banshee_node) ->
      try
        let n = BansheeNodeHT.find all_nodes x in
        let elt = find n in
        add_node elt s
      with Not_found -> s
    )
    set nl

let make_open_edge x y i = begin
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) "( %d %d %d\n" x.nid y.nid i;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  banshee_make_open_edge x.nnode y.nnode i;
end

let make_close_edge x y i = begin
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) ") %d %d %d\n" x.nid y.nid i;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  banshee_make_close_edge x.nnode y.nnode i;
end

let make_sub_edge x y = begin
  if !do_dump_dyckcfl then begin
    Printf.fprintf (Lazy.force dump_dyckcfl_file) "s %d %d\n" x.nid y.nid;
    flush (Lazy.force dump_dyckcfl_file);
  end;
  banshee_make_sub_edge x.nnode y.nnode;
end

let _ = bansheeInit true
