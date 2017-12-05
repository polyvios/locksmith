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
val options : (string * Arg.spec * string) list

type node
type instantiation

module InstHT : Hashtbl.S with type key = instantiation
module Node : Lockutil.HashedOrderedType with type t = node
module NodeSet : Set.S with type elt = node
module NodeHT : Hashtbl.S with type key = node

val make_node : string -> bool -> Cil.fundec option -> Cil.location -> bool -> node
val update_node_location : node -> Cil.location -> node
val string_of_node : node -> string
val dotstring_of_node : node -> string
val fresh_inst : unit -> instantiation
val make_open_edge : node -> node -> instantiation -> unit
val make_close_edge : node -> node -> instantiation -> unit
val make_sub_edge : node -> node -> unit
val set_global : node -> unit
val is_global : node -> bool
val reaches_m : node -> node -> bool
val is_concrete : node -> bool
val string_of_inst : instantiation -> string
val total_nodes : unit -> int
val get_all_that_reach_m : node -> (node -> 'a) ->
                           ('a -> 'b -> 'b) -> 'b -> 'b

val get_all_that_reach_pn : node -> (node -> 'a) ->
                            ('a -> 'b -> 'b) -> 'b -> 'b

(* call this to freeze the number of instantiations.
 * flow edges & nodes can still be added
 *)
val done_adding : unit -> unit
