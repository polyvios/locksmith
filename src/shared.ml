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
open Pretty
open Labelflow
open Lockutil
module LS = Lockstate
module E = Errormsg
module CF = Controlflow
type phi = CF.phi

let do_sharedreads = ref false
let do_list_shared = ref false
let down_fork = ref true
let debug = ref false
let const_only = ref true
let flow_compare = ref false
let flow_share = ref true

(* options:
    flow_compare implies flow_share and flow_effect
    flow_effect implies flow_share
*)

let set_compare () = begin
  flow_compare := true;
  flow_share := true;
  flow_effect := true
end

let set_noflow () = begin
  flow_share := false;
  flow_effect := false
end

let set_cs_flow () = begin
  flow_share := true;
  flow_effect := true;
end

let options = [
  "--debug-shared",
    Arg.Set(debug),
    " Debug the sharedness analysis";

  "--list-shared",
    Arg.Set(do_list_shared),
    " List shared locations.";

  "--shared-reads",
    Arg.Set(do_sharedreads),
    " Reads cause shared.";

  "--no-down-fork",
    Arg.Clear(down_fork),
    " Apply (Down) at fork calls";

  "--no-const-only",
    Arg.Clear(const_only),
    " Only consider constant locations for sharing";

  "--no-flow-share",
    Arg.Unit(set_noflow),
    " Do not determine location sharing flow-sensitively";

  "--cs-flow-share",
    Arg.Unit(set_cs_flow),
    " Determing sharing flow- and context-sensitively";

  "--flow-compare",
    Arg.Unit(set_compare),
    " Compare the relative precision of different sharing schemes";
]


(* local module state *)
let solved = ref false

type fork_info = 
    { input_eff : effect; (* input effect of parent *)
      output_eff : effect;(* (global) alias of parent output effect *) 
      fork_eff : effect;  (* (input) effect of thread *)
      input_phi : phi;              (* right before fork *)
      output_phi : phi;             (* right after fork (main thread) *)
      fork_phi : phi;               (* child thread *)
      live_vars : rhoSet Lazy.t; (* live vars at fork *)
      loc : Cil.location; (* file location of the fork *)
    }

let all_forks : fork_info list ref = ref []

let add_fork (in_eff: effect)
             (out_eff: effect)
             (fork_eff: effect)
             (main_phi: phi)
             (cont_phi: phi)
             (forked_phi: phi)
             (lvs: unit -> rhoSet): unit =
  assert (not !solved);
  (* This is conservative; we would be justified to prune flow
     from the forked thread into the parent if it was not in
     scope, but that would require a fixed-point and it's not
     easy to avoid introducing spurious flow.  So instead
     we just flow it directly, and hope that intersecting with
     the live variables to determine sharing will kill any
     spurious flow. *)
  effect_flows fork_eff in_eff;
  (* We make this global to avoid needing a PN query later.  Could
     ditch this and ask PN queries to solve e instead. *)
  let e = make_effect "e" true in
  effect_flows out_eff e;
  set_global_effect e EffectSet.empty;
  (* Add to the list of forks to process later *)
  all_forks := {
     input_eff = in_eff;
     output_eff = e;
     fork_eff = fork_eff;
     input_phi = main_phi;
     output_phi = cont_phi;
     fork_phi = forked_phi;
     live_vars = Lazy.lazy_from_fun lvs;
     loc = !Cil.currentLoc;
   } :: !all_forks

module SharingTransfer =
  struct
    type state = rhoSet
    let state_before_phi = CF.PhiHT.create 1000 
    let transfer_fwd (p: phi) worklist (shared: state) : state option = Some shared
    let starting_state (p: phi) : state option = Some RhoSet.empty
    let merge_state = RhoSet.union
    let equal_state = RhoSet.equal
    let translate_state_in (s: state) (i: instantiation) : state = s
    let translate_state_out (s: state) (i: instantiation) : state = s
    let check_state (p: phi) (s: state) : unit = ()
    let pretty () (s:state) = 
      align ++ d_rhoset () s ++ unalign
  end

module SS = CF.MakeForwardsAnalysis(SharingTransfer)

let get_state_before = CF.PhiHT.find SharingTransfer.state_before_phi
let set_state_before = CF.PhiHT.replace SharingTransfer.state_before_phi

let all_shared_rho: rhoSet ref = ref RhoSet.empty

let starting_phis : phi list ref = ref []

let set_shared (rs: rhoSet) (ps:phi list) (es:effect list): unit =
  assert (not !solved);
  let crs = if !const_only then concrete_rhoset rs else rs in
  all_shared_rho := RhoSet.union !all_shared_rho crs;
  if !flow_share then
    begin
      if !flow_effect then
        List.iter (function (e:effect) ->
          RhoSet.iter (function r -> add_to_share_effect r e) crs)
          es;
      if not !flow_effect || !flow_compare then
        List.iter (function (p:phi) ->
          set_state_before p crs;
          starting_phis := p::!starting_phis)
        ps
    end

let forkn = ref 0

let find_shared
    (all_global_rho: RhoSet.t)
     { output_eff = out_eff;
       fork_eff = fork_eff;
       live_vars = llvs;
       output_phi = ophi;
       fork_phi = fphi;
       loc = loc; } : unit = 
begin
  incr forkn; 
  let r1,w1 = solve_rw_effect_pn out_eff in
  if !debug then ignore(E.log "PARENT (%a) read: %a\nPARENT (%a) write: %a\n" 
                                Cil.d_loc loc d_rhoset r1 Cil.d_loc loc d_rhoset w1);
  let r2,w2 = 
   (* We intersect the pn-closed effect of the forked thread
     with the pn-closed fv(env) at that point. *)
    if !down_fork then 
      begin
        let fr,fw = solve_rw_effect_pn fork_eff in
        if !debug then ignore(E.log "FORK (%a) no filter read: %a\nFORK (%a) no filter write: %a\n" 
                                Cil.d_loc loc d_rhoset fr Cil.d_loc loc d_rhoset fw);
        let local_lvs = Lazy.force llvs in
        let closed_local_lvs = close_rhoset_pn local_lvs in
        let lvs = RhoSet.union closed_local_lvs all_global_rho in
        if !debug then 
          ignore(E.log "FV(Gamma) (%a): %a\n" Cil.d_loc loc d_rhoset lvs);
        (RhoSet.inter fr lvs,RhoSet.inter fw lvs)
      end
    else 
      solve_rw_effect_pn fork_eff in
  if !debug then ignore(E.log "FORK (%a) filtered read: %a\nFORK (%a) filtered write: %a\n"
                                Cil.d_loc loc d_rhoset r2 Cil.d_loc loc d_rhoset w2);
  let l1 = RhoSet.union r1 w1 in
  let l2 = RhoSet.union r2 w2 in
  let l3 =
    if !do_sharedreads then (RhoSet.inter l1 l2)
    else  
      let ls1 = (RhoSet.inter l1 w2) in
      let ls2 = (RhoSet.inter l2 w1) in
      let res = RhoSet.union ls1 ls2 in
      if !debug then ignore(E.log "FORK (%a) shared: %a\n\n"
                              Cil.d_loc loc d_rhoset res);
      res
  in
  (* add this to hash (input_before) for both forked and cont *)
  set_shared l3 [ophi;fphi] [out_eff;fork_eff]; 
  if !debug then ignore(E.log ".");
end

let dump_shared () : unit =
  assert (!solved);
  let cs = concrete_rhoset !all_shared_rho in
  ignore(E.log "shared: %a\n" d_rhoset cs);
  if !debug then begin
    ignore(E.log "sharedv %d / %d\n" (RhoSet.cardinal !all_shared_rho) (count_rho ()));
    ignore(E.log "sharedc %d / %d\n" (RhoSet.cardinal cs) (RhoSet.cardinal !all_concrete_rho));
    ignore(E.log "allc: %a\n" d_rhoset !all_concrete_rho)
  end

(* let memoized_shared_rho : bool RhoHT.t = RhoHT.create 1000 *)

let is_ever_shared (r: rho) : bool =
   assert (!solved);
(*    try  *)
(*      RhoHT.find memoized_shared_rho r *)
(*    with Not_found -> *)
     let rs = get_rho_p2set_pn r in
     let res = not (RhoSet.is_empty (RhoSet.inter rs !all_shared_rho)) in
(*      RhoHT.add memoized_shared_rho r res; *)
     res

(* module PhiRhoHT = *)
(*   struct *)
(*     type t = phi * rho *)
(*     let equal ((x,r): t) ((y,q): t) : bool = *)
(*       PhiHT.equal x y && Rho.equal r q *)
(*     let hash ((x,r): t) : int =  *)
(*       2 * (PhiHT.hash x) + (Rho.hash r) *)
(*   end *)
(* module PRhoHT = Hashtbl.Make(PhiRhoHT) *)

let is_shared (r: rho) (p: phi) (e:effect) : bool =
  assert (!solved);

  let is_shared' r shared_set =
    not (r = unknown_rho) &&
    (let rs = get_rho_p2set_pn r in
    not (RhoSet.is_empty (RhoSet.inter rs shared_set))) in

  if !flow_compare then
    begin
      let all_shared_eff = concrete_rhoset (solve_share_effect_pn e) in
      let all_shared = get_state_before p in
      let res = is_shared' r all_shared in
      let res_eff = is_shared' r all_shared_eff in
      let res_noflow = is_shared' r !all_shared_rho in
      if (res != res_eff) then
        begin
          assert(res = res_noflow);
          ignore(E.log "location %a (%a): CS/FS says %b while FS says %b\n"
                   d_rho r CF.d_phi p res_eff res);
          ignore(E.log "all_shared_eff: %a\nall_shared: %a\n"
                   d_rhoset all_shared_eff d_rhoset all_shared)
        end
      else if (res != res_noflow) then
        ignore(E.log "location %a: non-FS says %b while FS says %b\n"
                 d_rho r res_noflow res);
      res_eff
    end
  else if !flow_share then
     begin
       let all_shared =
         if !flow_effect then
           solve_share_effect_pn e
         else
           get_state_before p in
       is_shared' r all_shared
     end
  else
    is_ever_shared r

let solve (rs: RhoSet.t) : unit = begin
  assert (not !solved);
  if !debug then ignore(E.log "solving effects for %d forks\n" (List.length !all_forks));
  let closedrs = if !down_fork then (close_rhoset_pn rs) else RhoSet.empty in
  List.iter (find_shared closedrs) !all_forks;
  if !debug then ignore(E.log "\n");
  solved := true;
  (* calculate flow sensitive sharing here *)
  if (!flow_share && not (!flow_effect)) || !flow_compare then 
    begin
      (* let old = !CF.debug in
      if !debug then CF.debug := true; *)
      SS.solve (!CF.starting_phis @ !starting_phis);
      (* if !debug then CF.debug := old *)
    end;
(*   RhoSet.iter (function rho -> RhoHT.add memoized_shared_rho rho true)  *)
(*     !all_shared_rho; *)
  if !do_list_shared then dump_shared ()
end


