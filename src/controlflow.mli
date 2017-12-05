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
exception ControlFlowBug of string

val options : (string * Arg.spec * string) list
val debug : bool ref

(* program point *)
type phi

type phi_kind =
  | PhiVar
  | PhiForked
  | PhiPacked
  | PhiNewlock of Labelflow.lock
  | PhiAcquire of Labelflow.lock
  | PhiRelease of Labelflow.lock
  | PhiDelete of Labelflow.lock
  | PhiSplitCall of Labelflow.lock_effect * phi
  | PhiSplitReturn of Labelflow.lock_effect * phi

val set_phi_kind : phi -> phi_kind -> unit
val get_phi_kind : phi -> phi_kind

module Phi : Lockutil.HashedOrderedType with type t = phi
(* a hashtable from phi to 'a.
 * it is faster than (phi, 'a) Hashtbl.t,
 * because it uses phi_id to compare
 *)
module PhiHT : Hashtbl.S with type key = phi
module PhiSet : Set.S with type elt = phi
type phiSet = PhiSet.t

(* all phi nodes in the control flow graph *)
val all_phi : phi list ref

(* "empty" program point.
 * Auxiliary phi used to type effect-less expressions
 *)
val empty_phi : phi

(*********************
 * graph construction
 *********************)

(* creates a fresh phi node, corresponding to !currentLoc program point,
 *)
val make_phi : string -> phi

(* info *)
val function_of_phi : phi -> Cil.fundec option
val location_of_phi : phi -> Cil.location

val starting_phis : phi list ref
(*val phi_calls : phi -> phi -> phi -> Labelflow.effect -> unit*)

(* creates a control flow edge from phi1 to phi2
 * fails if any of the two is the empty_phi
 *)
val phi_flows : phi -> phi -> unit

(* create an instantiation edge "control enters context" from the first
 * to the second argument at the given instantiation site.
 * polarity (not used (?) ) is true for positive.
 *)
val inst_phi : phi -> phi -> bool -> Labelflow.instantiation -> unit

(* Marks a phi point as global, unless it is in the set qs.
 * Global phis are the ones anottating global function pointers,
 * for example.
 *)
val set_global_phi : phi -> phiSet -> unit

(* unify two phi program points.
 * equivalent to each flowing to the other
 *)
val unify_phi : phi -> phi -> unit

(* return a new phi to which both p1 and p2 flow *)
val join_phi : phi -> phi -> phi

(*******************************************
 * utilities for traversal of the phi graph 
 *******************************************)

(* return all the "next" phis starting from this.
 * The returned list will include all next, open parenthesis
 * and close parenthesis edges.
 *)
(*
 * POLYVIOS: I only expect the postorder traversal in
 * correlation.ml to call this function.
 *)
val get_all_children : phi -> phi list
val postorder_visit : (phi -> unit) -> unit

(******************
 * pretty printing
 ******************)
(* phi2string *)
val dotstring_of_phi : phi -> string
val d_phi : unit -> phi -> Pretty.doc

(* phiSet formatting *)
val d_phiset : unit -> phiSet -> Pretty.doc

val print_graph : out_channel -> (phi -> bool) -> unit

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

module ForwardsWorklist : PhiWorklist

module type ForwardsTransfer =
  sig
    type state
    val state_before_phi : state PhiHT.t
    val transfer_fwd : phi -> ForwardsWorklist.t -> state -> state option
    val starting_state : phi -> state option
    val merge_state: state -> state -> state
    val equal_state: state -> state -> bool
    val translate_state_in: state -> Labelflow.instantiation -> state
    val translate_state_out: state -> Labelflow.instantiation -> state
    val check_state: phi -> state -> unit
    val pretty : unit -> state -> Pretty.doc
  end

module type ForwardsAnalysis =
  sig
    type state
    val solve : phi list -> unit
  end

module MakeForwardsAnalysis:
  functor (A: ForwardsTransfer) -> ForwardsAnalysis with type state = A.state

module BackwardsWorklist : PhiWorklist

module type BackwardsTransfer =
  sig
    type state
    val state_after_phi : state PhiHT.t
    val transfer_back : phi -> BackwardsWorklist.t -> state -> state
      (* The transfer function.  It is given the current phi, the worklist of phis
       * and the current state before the phi.  It should return the state after
       * the phi.  The worklist is given in case the transfer function affects other
       * phis that are not directly related to this phi.  Usually it's ignored.
       *)

    val is_relevant : phi -> bool
      (* A "filter" function.  We use this to simplify the control flow graph.
       * Whenever there is a simple edge between two "irrelevant" phis, we
       * unify them into one phi.  This is a prepass, before the actual
       * fixpoint analysis is computed.
       *)

    val starting_state : phi -> state
      (* We use this function to set the initial state per phi.
       * It usually should return top or bottom,  unless there's a special
       * initial value for the given phi.
       *)

    val merge_state: state -> state -> state
      (* merge function.  It's given two states, and should return one that
       * is their merge.
       *)

    val equal_state: state -> state -> bool
    val translate_state_in: state -> Labelflow.instantiation -> state
    val translate_state_out: state -> Labelflow.instantiation -> state
    val check_state: phi -> state -> unit
    val pretty : unit -> state -> Pretty.doc
  end

(* This is the type you get back by calling MakeBackwardsAnalysis with
 * some transfer module.  Calling solve computes the analysis fixpoint.
 * In order to get the result, the caller should use the state_after_phi
 * hashtable of the transfer-function module
 *)
module type BackwardsAnalysis =
  sig
    type state
    val solve : phi list -> unit
  end

module MakeBackwardsAnalysis:
  functor (A: BackwardsTransfer) ->
    BackwardsAnalysis with type state = A.state

(* Print how many times phi nodes have been visited during the analysis.
 * Currently, only counts backwards analysis.  Intended to count how
 * many times guarded-by visits every phi.
 * It takes an argument, a function that returns whatever to be printed
 * after the count per phi.
 *)
val count_phi_visits : (phi -> string) -> unit
