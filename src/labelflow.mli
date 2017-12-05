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
type node (* all kinds of labels *)
type lock (* lock label. can be concrete, or variable. variables have
           * a solution, a set of labels that flow there.
           *)
type rho  (* location label.  can be concrete. *)
type effect (* continuation effect label, cannot be concrete.
             * solution includes a pair of rhosets, the read and write
             * effects
             *)
type chi (* scoped effect label.  cannot be concrete.  is exactly like
          * a continuation effect label, except it doesn't refer to
          * the continuation, but to "standard" effects
          *)
type lock_effect (* lock effect label, cannot be concrete.  its
                  * solution is all the locks that are touched in a
                  * function (annotates function type)
                  *)
type instantiation

val options : (string * Arg.spec * string) list

(* determines whether we are doing flow-sensitive sharing using
   effects.  Also referred to in shared.ml *)
val flow_effect : bool ref

module RhoSet : Set.S with type elt = rho
module LockSet : Set.S with type elt = lock
module Effect : Set.OrderedType with type t = effect
module EffectSet : Set.S with type elt = effect
module EffectMap : Map.S with type key = effect

(* sets of labels, per kind of label *)
type rhoSet = RhoSet.t  (* set of rho labels *)
type lockSet = LockSet.t (* set of lock labels *)
type effectSet = EffectSet.t (* set of effect labels *)

(* various hashtables *)
module LockHT : Hashtbl.S with type key = lock
module RhoHT : Hashtbl.S with type key = rho
module Rho : Hashtbl.HashedType with type t = rho
module Inst : Hashtbl.HashedType with type t = instantiation
module InstHT : Hashtbl.S with type key = instantiation

(*********************
 * graph construction
 *********************)

(* call this after adding all instantiations needed.  It fills the self-loops
 * for globals.  Other nodes/constraints can still be added.
 *)
val done_adding : unit -> unit

(* instantiations *)

(* Creates an instantiation marker.
 * Instantiations are marked with an effect variable.
 * Only the "lock" effect of the variable is used in order
 * to skip "translating" irrelevant locks through that instantiation.
 * the bool is true for "exists" (pack) instantiations and
 * false for "forall" ones.
 *)
val make_instantiation : bool -> string -> instantiation

(* graph construction statistics *)
val get_stats: unit -> string

(* rhos *)

(* creates a fresh rho label labeled "s".
 * If concrete=true, the fresh rho is marked as "constant", i.e. it
 * corresponds to a malloc() or "&" memory allocation
 *)
val make_rho : Labelname.label_name -> bool -> rho
val update_rho_location : rho -> Cil.location -> rho

(* how many rhos have been created *)
val count_rho : unit -> int

(* mark r as global *)
val set_global_rho : rho -> unit

(* A special constant used to denote a lock with no name
 * in the current context.  It is the result of any substitution that's
 * not defined.
 *)
val unknown_rho : rho

(* create a subtyping edge from r1 to r2 *)
val rho_flows : rho -> rho -> unit
(*val lock_flows : lock -> lock -> unit*)

(* creates a unification constraint between x and y.
 * is equivalent with two subtyping constraints
 *)
val unify_rho : rho -> rho -> unit

(* returns the join of two rhos.
 * a fresh rho to which they both flow
 *)
(*val join_rho : rho -> rho -> rho*)

(* instantiate the first argument to the second with positive (true) or
 * negative(false) polarity at the given instantiation *)
val inst_rho : rho -> rho -> bool -> instantiation -> unit


(* locks *)

(* creates a fresh lock with the given name.
 * If concrete = true, the lock is marked as concrete, i.e. corresponds to
 * a call to newlock.
 *)
val make_lock : Labelname.label_name -> bool -> lock

(* unknown lock.  used to denote locks not visible in the current context *)
val unknown_lock : lock

(* empty lock.  should never include any concrete lock in its solution *)
val empty_lock : lock

(* mark lock as global (add all selfloops *)
val set_global_lock : lock -> unit

(* instantiate (with given polarity) the first argument
 * to the second at the given instantiation *)
val inst_lock : lock -> lock -> instantiation -> unit

(* unify two lock variables *)
val unify_locks : lock -> lock -> unit

(* create a fresh _CONCRETE_ lock given a _CONCRETE_ lock and an
 * instantiation, that corresponds to a fresh concrete lock for that
 * instantiation
 *)
val clone_lock : lock -> instantiation -> lock

(* mark a lock as non-linear *)
val set_nonlinear : lock -> unit

(* checks to see if a lock label is linear.
 * Safe to use after linearity phase is done.
 *)
val is_nonlinear : lock -> bool


(* normal effects *)
val make_chi : string -> chi
val add_to_read_chi : rho -> chi -> unit
val set_global_chi : chi -> unit
val chi_flows : chi -> chi -> unit
val inst_chi : chi -> chi -> bool -> instantiation -> unit
val solve_chi_m : chi -> (rhoSet * rhoSet)
val solve_chi_pn : chi -> (rhoSet * rhoSet)
val add_to_write_chi : rho -> chi -> unit
val dump_all_chi : unit -> unit

(* effects *)

(* create an effect variable \varepsilon.  it captures the read and write
 * effects and the locks that are "touched".
 *)
val make_effect : string -> bool(* set to true to use tagged node *) -> effect
val make_lock_effect : unit -> lock_effect
val inst_lock_effect : lock_effect -> lock_effect -> bool -> instantiation -> unit
val lock_effect_flows : lock_effect -> lock_effect -> unit
val set_global_lock_effect : lock_effect -> unit

(* empty effect.  used to type effect-less expressions. Bug will be raised
 * if something is effected in it
 *)
val empty_effect : effect

(* marks global effect (e.g. effect of global function ptr),
 * unless e is in the set qs
 *)
val set_global_effect : effect -> effectSet -> unit

(* create a subtyping edge from e1 to e2.  This usually means that
 * e2 corresponds to a program point BEFORE e1. (effects go backwards)
 *)
val effect_flows : effect -> effect -> unit

(* create a subset edge from chi to effect.  includes all read and
 * write effects in chi into the solution of the continuation effect
 * used in typing function call, to match the type rule in the
 * contextual effects paper 
 *)
val chi_in_effect : chi -> effect -> unit

(* instantiation effect eabs to einst with polarity "polarity" at
 * instantiation site i
 *)
val inst_effect : effect -> effect -> bool -> instantiation -> unit

(* create an "effect-membership" edge: loc is read in ef *)
val add_to_read_effect : rho -> effect -> unit

(* create an "effect-membership" edge: loc is written in ef *)
val add_to_write_effect : rho -> effect -> unit

(* create an "effect-membership" edge: loc is shared bt/w threads in ef *)
val add_to_share_effect : rho -> effect -> unit

(* create an "effect-membership" edge: l is created, acquired,
 * released or destroyed in ef
 *)
val add_to_lock_effect : lock -> lock_effect -> unit

(* return the join of two effects
 * this works backwards, the result flows to both inputs
 *)
val join_effects : effect -> effect -> effect

(* unify two effect variables *)
val unify_effects : effect -> effect -> unit

(******************
 * solving queries
 ******************)

val is_concrete_lock : lock -> bool

val is_global_lock : lock -> bool

val is_global_rho : rho -> bool

(*val lock_equal : lock -> lock -> bool*)
(*val rho_equal : rho -> rho -> bool*)
val inst_equal : instantiation -> instantiation -> bool

(* return a set of locks that the argument might point to *)
val get_lock_p2set : lock -> lockSet

(* return a set of rhos that the argument might point to *)
val get_rho_p2set_m : rho -> rhoSet
val get_rho_p2set_pn : rho -> rhoSet

(* close a set with respect to "flows to" *)
val close_rhoset_m : rhoSet -> rhoSet
val close_rhoset_pn : rhoSet -> rhoSet

val close_lockset : lockSet -> lockSet

(* return the subset of concrete rhos *)
val concrete_rhoset : rhoSet -> rhoSet

(* return the subset of concrete and linear locks *)
val concrete_lockset : lockSet -> lockSet

(* return two sets of rhos, for read and write effects *)
val solve_rw_effect_pn : effect -> (rhoSet * rhoSet)
(*val solve_rw_effect_m : effect -> (rhoSet * rhoSet)*)
val solve_share_effect_pn : effect -> rhoSet

(* translate a lockset positively through the given instantiation
 * (outwards, abstract becomes instance)
 *)
val translate_lockset_out : instantiation -> lockSet -> lockSet

(* translate a lockset negatively through the given instantiation
 * (inwards, instance becomes abstract)
 *) 
val translate_lockset_in : instantiation -> lockSet -> lockSet

(* translate a rhoset positively through the given instantiation
 * (outwards, abstract -> instance)
 *) 
val translate_rhoset_out : instantiation -> rhoSet -> rhoSet

(* inwards *)
val translate_rhoset_in : instantiation -> rhoSet -> rhoSet

(* return the subset of ls that won't be translated through i
 * (all that don't reach the effect)
 *)
(*val untranslated_lockset_in : instantiation -> lockSet -> lockSet*)

(* split a lockset into two, containing locks that are and are not
 * in the given effect
 *)
val split_lockset : lockSet -> lock_effect -> lockSet * lockSet

(* return true if the instantiation is due to an existential pack site *)
val is_pack : instantiation -> bool

(* iterate f over all instantiations *)
val inst_iter : (instantiation -> unit) -> unit

val concrete_rho_iter : (rho -> unit) -> unit
val all_concrete_rho : rhoSet ref

(* call to assert all non-linear locks are marked so *)
val close_nonlinear : unit -> unit


(******************
 * pretty printing
 ******************)
(* dot-string. format in a way that's best for "dot" graph drawing *)
val dotstring_of_lock_effect : lock_effect -> string
val dotstring_of_lock : lock -> string
val dotstring_of_rho : rho -> string
val dotstring_of_inst : instantiation -> string
val dotstring_of_read_effect : effect -> string
val dotstring_of_write_effect : effect -> string

val d_lock : unit -> lock -> Pretty.doc
val d_rho : unit -> rho -> Pretty.doc

(* lockSet formatting *)
val d_lockset : unit -> lockSet -> Pretty.doc

(* rhoSet formatting *)
val d_rhoset : unit -> rhoSet -> Pretty.doc

val d_rhopath : unit -> (rho * rho) -> Pretty.doc

(* effect formatting *)
val d_effect : unit -> effect -> Pretty.doc

val d_chi : unit -> chi -> Pretty.doc

(* effectSet formatting *)
val d_effectset : unit -> effectSet -> Pretty.doc

(* instantiation formatting. use string_of_inst if you just want the index *)
val d_instantiation : unit -> instantiation -> Pretty.doc


(***************
 * graph output
 ***************)

val print_graph : out_channel -> unit
