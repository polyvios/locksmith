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
type node = int
type instantiation = int

module Node =
  struct
    type t = node
    let compare = (-)
    let equal = (=)
    let hash i = i
  end
module NodeSet = Set.Make(Node)
module NodeHT = Hashtbl.Make(Node)

let options = []
let next_id = ref 1
let make_node a b c d =
  let r = !next_id in incr next_id; r
let string_of_node node = "a"
let dotstring_of_node node = "a"
let fresh_inst () = 1
let make_inst_edge node1 node2 polarity instantiation = ()
let make_sub_edge node node = ()
let set_global node = ()
let is_global node = true
let reaches node1 node2 = true
let reaches_m node1 node2 = true
let print_graph out_channel = NodeSet.empty
let hash node = 1
let is_concrete node = true
let doFullCFL () = ()
let string_of_inst instantiation = "i"
let total_nodes () = 1
let get_all_that_reach_m _ _ _ b = b
let get_all_that_reach_pn _ _ _ b = b
let done_adding () = ()
