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
(*
type epsilon_kind =
  | EEmpty
  | ESingleton of Labelflow.lock
  | EChi
  | EUplus of epsilon * epsilon
  | EUnion of epsilon * epsilon

and epsilon
*)

type epsilon

val empty_epsilon : epsilon

(*
val make_epsilon : epsilon_kind -> epsilon
*)

val singleton : Labelflow.lock -> epsilon

val union : epsilon -> epsilon -> epsilon

val uplus : epsilon -> epsilon -> epsilon

val make_var_epsilon : unit -> epsilon

(* add a constraint e \leq x *)
val epsilon_flow : epsilon -> epsilon -> unit

(* add a constraint e \leq_l x (down-rule) *)
val epsilon_filter : epsilon -> (Labelflow.lockSet*Labelflow.rhoSet)
                     -> epsilon -> unit

(* add an instantiation constraint *)
val epsilon_inst : epsilon -> Labelflow.instantiation -> epsilon -> unit

val epsilon_global : epsilon -> unit

val solve : unit -> unit

(* check solution for linearity *)
val check : unit -> unit

(* scan all escapes() constraints and print errors if some don't hold *)
(*val check_escapes : unit -> unit*)

val print_graph : out_channel -> unit
val d_epsilon : unit -> epsilon -> Pretty.doc

val options : (string * Arg.spec * string) list
