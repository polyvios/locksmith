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

module StringSet = Set.Make(String)

(*****************************************************************************)

let debug = ref false
let pthread_h_name = ref "/usr/include/pthread.h"

let options =
  [
    "--debug-lockpick",
      Arg.Set(debug),
      " Print lockpick profiling information after each phase.";

    "--pthread-h",
      Arg.String(fun s -> pthread_h_name := s),
      " Specify where pthread.h is.";
  ]

module LockRewriter = struct
  let lock_function : varinfo option ref = ref None
  let unlock_function : varinfo option ref = ref None
  let init_function : varinfo option ref = ref None
  let lock_type : typ option ref = ref None

  let init file = begin
    lock_function := Some (find_function_var file "pthread_mutex_lock");
    unlock_function := Some (find_function_var file "pthread_mutex_unlock");
    init_function := Some (find_function_var file "pthread_mutex_init");
    lock_type := Some (find_type file "pthread_mutex_t");
  end

  let changeLockType lt ifun = begin
      init_function := ifun;
      lock_type := lt;
  end

  let make_wrapper (f: fundec) (locks: VS.t) : fundec = begin
    (* make an empty function *)
    let f_new = Cil.emptyFunction (f.svar.vname ^ "__atomic") in
    (* declare its argument names and types *)
    Cil.setFunctionTypeMakeFormals f_new f.svar.vtype;
    (* create a new local variable with type equal to the return type *)
    let ret_type = get_return_type f.svar.vtype in
    let arguments: exp list = List.map (fun v -> Lval(Cil.var v)) f_new.sformals in
    let ret_lval, ret_stmt = 
      if isVoidType ret_type then (
        None, Cil.mkStmt (Return(None, Cil.locUnknown))
      ) else (
        let tmp_var: varinfo = Cil.makeTempVar f_new ~name:"result" ret_type in
        let ret_val: lval = (Cil.var tmp_var) in
        Some ret_val, Cil.mkStmt (Return (Some (Lval ret_val), Cil.locUnknown))
      )
    in
    let i = Call(ret_lval, Lval(Cil.var f.svar), arguments, Cil.locUnknown) in
    let lock = var (getSome !lock_function) in
    let unlock = var (getSome !unlock_function) in
    let acq, rel = VS.fold
      (fun l (acq, rel) -> 
        let acql = Call(None, Lval(lock), [Cil.mkAddrOf (Cil.var l)], Cil.locUnknown) in
        let rell = Call(None, Lval(unlock), [Cil.mkAddrOf (Cil.var l)], Cil.locUnknown) in
        (acql::acq, rell::rel)
      )
      locks
      ([], [])
    in
    let instr_list: instr list = acq @ [i] @ (List.rev rel) in
    let stmt = Cil.mkStmt (Instr instr_list) in
    f_new.sbody.bstmts <- f_new.sbody.bstmts @ [stmt; ret_stmt];
    f_new
  end

  (* get a file, a fundec of an atomic function in that file (along with it's
   * chi effect) and make it atomic
   *)
  let make_atomic_function (f: file) (rho_to_lock: varinfo RhoHT.t) (fd: fundec) (rs: RhoSet.t) : unit = begin
    if !debug then (
      ignore(E.log " %s : { " fd.svar.vname;
	let lockStrs = RhoSet.fold 
	  (fun r acc ->
	     let s = Pretty.sprint 800 (d_lval () (Cil.var (RhoHT.find rho_to_lock r))) in 
	       StringSet.add s acc) 
	  rs StringSet.empty in 
	  StringSet.iter (fun s -> ignore(E.log "%s " s)) lockStrs;
	  ignore(E.log "}\n"))
    );
    let locks = RhoSet.fold (fun r acc -> VS.add (RhoHT.find rho_to_lock r) acc) rs VS.empty in
    let mw = make_wrapper fd locks in
    change_var f fd.svar mw.svar;
    add_after f fd mw;
  end

  let generate_locks file shared_atomic solution used = begin
    (* Create a Hash Table mapping rho to varinfos of locks *)
    let rho_to_lock = RhoHT.create (RhoSet.cardinal shared_atomic) in
    (* Create a fresh lock for each rho in 'used', and add it at the end of the file *)
    let lock_count = ref 0 in
    RhoSet.iter
      (fun r ->
          let lock = Cil.makeGlobalVar ("__lockpick__lock__"^(string_of_int !lock_count)) (getSome !lock_type) in
          file.globals <- file.globals @ [GVar(lock,{init = None},Cil.locUnknown)];
          assert (not (RhoHT.mem rho_to_lock r));
          RhoHT.add rho_to_lock r lock;
          incr lock_count;
       )
      used;
    (* add the rest to the HT *)
    RhoSet.iter
      (fun r ->
          let rl = RhoHT.find solution r in
          RhoHT.replace rho_to_lock r (RhoHT.find rho_to_lock rl))
      shared_atomic;
    (* print *)
    if !debug then
      (ignore(E.log "all locks:");
      RhoSet.iter (fun r -> ignore(E.log "%a " d_lval (Cil.var (RhoHT.find rho_to_lock r)))) used;
      ignore(E.log("\n")));
    (* Initialize all locks in the global init function *)
    let globInitFun = Cil.getGlobInit file in
    let init = var (getSome !init_function) in
    let initInstrs = RhoSet.fold 
      (fun r instrs -> 
	let l = RhoHT.find rho_to_lock r in
	let c = Call(None, Lval(init), [Cil.mkAddrOf (Cil.var l);Cil.zero], Cil.locUnknown) in
	c::instrs
      ) 
      used []
    in
    let stmt = Cil.mkStmt (Instr initInstrs) in
    globInitFun.sbody.bstmts <- globInitFun.sbody.bstmts @ [stmt];
    rho_to_lock
  end
end

let solve_atomic_chi (atomic_functions: (Cil.fundec, chi) Hashtbl.t)
                     (shared: RhoSet.t) : (Cil.fundec, RhoSet.t) Hashtbl.t =
  let h = Hashtbl.create (Hashtbl.length atomic_functions) in
  Hashtbl.iter
    (fun fd x -> let r,w = solve_chi_pn x in
      let result = concrete_rhoset (RhoSet.inter (RhoSet.union r w) shared) in
      if !debug then ignore(E.log "chi: %a has effect: %a\n" d_chi x d_rhoset result);
      Hashtbl.add h fd result
    )
    atomic_functions;
  h

(* Infer locking needed to enforce the atomic functions in the file, but don't
take into account the shared data that only exists in the ignore_function list.
Return a Hashtbl from Cil.fundec to a set of locks. *)
let infer_locks (f:file) (lock_type) (init_fun) (ignore_functions: (Cil.fundec,unit) Hashtbl.t) =
  Locktype.generate_constraints f;
  Labelflow.done_adding ();
  Shared.solve (Locktype.get_global_var_rhos ());

  let shared = concrete_rhoset !Shared.all_shared_rho in
  let atomic_functions = solve_atomic_chi LT.atomic_functions shared in
  let good_atomic = Hashtbl.copy atomic_functions in
  Hashtbl.iter (fun fd _ -> Hashtbl.remove good_atomic fd) ignore_functions;
  let shared_atomic = Hashtbl.fold (fun _ -> RhoSet.union) good_atomic RhoSet.empty
  in
  let solution = Lockalloc.solve atomic_functions shared_atomic in
  let used = RhoHT.fold (fun _ -> RhoSet.add) solution RhoSet.empty in
  if !debug then (
    ignore(E.log "atomic sections    : %d\n" (Hashtbl.length atomic_functions));
    ignore(E.log "shared locations   : %d\n" (RhoSet.cardinal shared));
    ignore(E.log "shared atomic loc. : %d\n" (RhoSet.cardinal shared_atomic));
    ignore(E.log "used locks         : %d\n" (RhoSet.cardinal used));
  );
  LockRewriter.init f;
  LockRewriter.changeLockType lock_type init_fun;

  let rho_to_lock = LockRewriter.generate_locks f shared_atomic solution used in
  let fun_to_locks = Hashtbl.create 1 in
  Hashtbl.iter (fun fd rs ->
    let locks = RhoSet.fold (fun r acc -> 
      if RhoHT.mem rho_to_lock r then VS.add (RhoHT.find rho_to_lock r) acc else acc)
      rs VS.empty 
    in Hashtbl.add fun_to_locks fd locks)
    atomic_functions;
  fun_to_locks


let doit (f: file) : unit = begin
  Rmtmps.removeUnusedTemps f;
  Rmalias.removeAliasAttr f;
  Locktype.generate_constraints f;
  Labelflow.done_adding ();
  Shared.solve (Locktype.get_global_var_rhos ());

  let shared = concrete_rhoset !Shared.all_shared_rho in
  let atomic_functions = solve_atomic_chi LT.atomic_functions shared in
  let shared_atomic =
    Hashtbl.fold (fun _ -> RhoSet.union) atomic_functions RhoSet.empty
  in
  let solution = Lockalloc.solve atomic_functions shared_atomic in
  let used = RhoHT.fold (fun _ -> RhoSet.add) solution RhoSet.empty in

  if !debug then (
    Lockalloc.dump_solution solution;
    ignore(E.log "atomic sections    : %d\n" (Hashtbl.length atomic_functions));
    ignore(E.log "shared locations   : %d\n" (RhoSet.cardinal shared));
    ignore(E.log "shared atomic loc. : %d\n" (RhoSet.cardinal shared_atomic));
    ignore(E.log "used locks         : %d\n" (RhoSet.cardinal used));
  );

  preprocessAndMergeWith f !pthread_h_name;
  LockRewriter.init f;
  let rho_to_lock = LockRewriter.generate_locks f shared_atomic solution used in
  Hashtbl.iter
    (LockRewriter.make_atomic_function f rho_to_lock)
    atomic_functions;
  Rmtmps.removeUnusedTemps f;
end


let feature : featureDescr = {
  fd_name = "lockpick";
  fd_enabled = ref false;
  fd_description = "lockpick";
  fd_extraopt = options;
  fd_doit = doit;
  fd_post_check = true;
}
