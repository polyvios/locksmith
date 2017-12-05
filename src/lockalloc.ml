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
open Cil
open Pretty
open Labelflow
open Lockutil
module Lprof = Lockprofile
module VS = Usedef.VS
module LT = Locktype

(*****************************************************************************)

let debug = ref false

let compute_dominates_graph (shared: RhoSet.t)
                            (atomic_functions: (fundec, RhoSet.t) Hashtbl.t)
                            : RhoSet.t RhoHT.t =
  let dominates_hash = RhoHT.create (RhoSet.cardinal shared) in
  let does_not_dominate r r' =
    if !debug then ignore(E.log "%a does not dominate %a\n" d_rho r d_rho r');
    let tmp = RhoHT.find dominates_hash r in
    RhoHT.replace dominates_hash r (RhoSet.remove r' tmp)
  in
  RhoSet.iter (fun r -> RhoHT.replace dominates_hash r shared) shared;
  Hashtbl.iter
    (fun _ atomic ->
      let notdom = RhoSet.diff shared atomic in
      RhoSet.iter
        (fun r -> RhoSet.iter (fun r' -> does_not_dominate r' r) notdom)
        atomic)
    atomic_functions;
  dominates_hash

let dump_dominates_graph (g: RhoSet.t RhoHT.t) : unit =
  ignore(E.log "dominates graph:\n");
  RhoHT.iter
    (fun r d ->
      RhoSet.iter
        (fun r' -> ignore(E.log "%a dominates %a\n" d_rho r d_rho r'))
        d
    ) g

let dump_solution (s: rho RhoHT.t) : unit =
  ignore(E.log "dominates:\n");
  RhoHT.iter
    (fun r r' -> ignore(E.log "%a is protected by %a\n" d_rho r d_rho r'))
    s

let solve (atomic_functions: (Cil.fundec, RhoSet.t) Hashtbl.t)
          (shared: RhoSet.t) : rho RhoHT.t = begin
  let dom = compute_dominates_graph shared atomic_functions in
  if !debug then dump_dominates_graph dom;
  (* algorithm 2 in the paper: *)
  let solution = RhoHT.create (RhoSet.cardinal shared) in
  RhoSet.iter (fun r -> RhoHT.replace solution r r) shared;
  RhoHT.iter
    (fun r dominated -> 
      RhoSet.iter
        (fun r' ->
          let s = RhoHT.find solution r in
          RhoHT.replace solution r' s;
          RhoHT.replace dom r' RhoSet.empty;
        )
        dominated
    )
    dom;
  solution
end
