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
val add_fork :
  Labelflow.effect -> (* main-thread input effect *)
  Labelflow.effect -> (* main-thread continuation effect *)
  Labelflow.effect -> (* forked-thread effect *)
  Controlflow.phi -> (* phi right before fork *)
  Controlflow.phi -> (* phi right after fork (main thread) *)
  Controlflow.phi -> (* starting phi of the new thread *)
  (unit -> Labelflow.rhoSet) -> (* thunk for calculating live vars *)
  unit

(* takes as input the set of global rhos *)
val solve : Labelflow.RhoSet.t -> unit
val is_shared : Labelflow.rho -> Controlflow.phi -> Labelflow.effect -> bool
val is_ever_shared : Labelflow.rho -> bool

(* do not change this, read-only. only valid after calling solve() *)
val all_shared_rho : Labelflow.rhoSet ref

(* debugging, dumps a list of shared rhos to stderr *)
val dump_shared : unit -> unit

(* if set to true, include read/read sharedness *)
(*val do_sharedreads : bool ref*)

val options : (string * Arg.spec * string) list
