(*
 *
 * Copyright (c) 2004-2007, 
 *  Nick Kuilema        <nicnak@cs.umd.edu>
 *  Polyvios Pratikakis <polyvios@cs.umd.edu>
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
open Lockutil
module LP = Lockpick
module LF = Labelflow
module LT = Locktype

(*****************************************************************************)

let debug = ref false
let use_mutex = ref false
let stm_h_name = ref "stm.h"
let pthread_h_name = ref "/usr/include/pthread.h"

let options =
  [
    "--debug-stm",
      Arg.Set(debug),
      " Print stm debugging info.";

    "--use-mutex-locks",
      Arg.Set(use_mutex),
      " Use mutexs instead of read/write locks for guarding irrevocable sections within the STM";

    "--stm-h",
      Arg.String(fun s -> stm_h_name := s),
      " Specify where stm.h is.";

    "--pthread-h",
      Arg.String(fun s -> pthread_h_name := s),
      " Specify where pthread.h is."

  ]

module STMRewriter = struct
  let addThread_function : varinfo option ref = ref None
  let removeThread_function : varinfo option ref = ref None
  let startTx_function : varinfo option ref = ref None
  let commitTx_function : varinfo option ref = ref None
  let read_function : varinfo option ref = ref None
  let write_function : varinfo option ref = ref None
  let stmEnv_type : typ option ref = ref None
  let word_type : typ option ref = ref None
  let addr_type : typ option ref = ref None

  let rd_lock_function : varinfo option ref = ref None
  let wr_lock_function : varinfo option ref = ref None
  let unlock_function : varinfo option ref = ref None
  let init_lock_function : varinfo option ref = ref None
  let lock_type : typ option ref = ref None


  let stmEnviron : Cil.exp option ref = ref None
    
  let init file = begin
    addThread_function := Some (find_function_var file "STMAddThread");
    removeThread_function := Some (find_function_var file "STMRemoveThread");
    startTx_function := Some (find_function_var file "STMStartTransaction");
    commitTx_function := Some (find_function_var file "STMCommitTransaction");
    read_function := Some (find_function_var file "STMReadValue");
    write_function := Some (find_function_var file "STMWriteValue");
    stmEnv_type := Some(Cil.voidPtrType); (* get #define expansion via temp file *)
    word_type := Some (find_type file "word_t");
    addr_type := Some (find_type file "addr_t");

    if !use_mutex then (
	rd_lock_function := Some (find_function_var file "pthread_mutex_lock");
	wr_lock_function := Some (find_function_var file "pthread_mutex_lock");
	unlock_function := Some (find_function_var file "pthread_mutex_unlock");
	init_lock_function := Some (find_function_var file "pthread_mutex_init");
	lock_type := Some (find_type file "pthread_mutex_t");
      ) else (
	rd_lock_function := Some (find_function_var file "pthread_rwlock_rdlock");
	wr_lock_function := Some (find_function_var file "pthread_rwlock_wrlock");
	unlock_function := Some (find_function_var file "pthread_rwlock_unlock");
	init_lock_function := Some (find_function_var file "pthread_rwlock_init");
	lock_type := Some (find_type file "pthread_rwlock_t");
    );
    (*stmEnv_type := Some (TVoid ([Attr ("thread",[])])); (* Need to add __thread attribute some how? *) *)
    let stmEnv = Cil.makeGlobalVar "__lockpick__stmEnv__" (getSome !stmEnv_type) in
    stmEnviron := Some (Cil.mkAddrOf (Cil.var(stmEnv)));
    file.globals <- file.globals @ [GVar(stmEnv,{init = None},Cil.locUnknown)];
  end

  let initGlobInit file = begin
    (* Create a global init function that calls the STM init function *)
    let stmInit = var (find_function_var file "STMInit") in
    let globInitFun = Cil.getGlobInit file in
    let init = Call(None, Lval(stmInit), [], Cil.locUnknown) in
    let addTh = Call(None, Lval(var(getSome !addThread_function)), [getSome !stmEnviron], Cil.locUnknown) in
    let stmt = Cil.mkStmt (Instr [init;addTh]) in
    globInitFun.sbody.bstmts <- globInitFun.sbody.bstmts @ [stmt]
  end

  let get_lock_type () = !lock_type
  let get_init_fun () = !init_lock_function

  let make_stm_wrapper (f: fundec) (stmFun: fundec) (locks: Usedef.VS.t) (lock: Cil.lval): fundec = begin
    (* make an empty function *)
    let f_new = Cil.emptyFunction (f.svar.vname ^ "__atomic") in
    (* declare its argument names and types *)
    Cil.setFunctionTypeMakeFormals f_new f.svar.vtype;
    (* create a new local variable with type equal to the return type *)
    let ret_type = get_return_type f.svar.vtype in
    let arguments = List.map (fun v -> Lval(Cil.var v)) f_new.sformals in
    let ret_lval, ret_stmt = 
      if isVoidType ret_type then (
        None, Cil.mkStmt (Return(None, Cil.locUnknown))
      ) else (
        let tmp_var: varinfo = Cil.makeTempVar f_new ~name:"retVal" ret_type in
        let ret_val: lval = (Cil.var tmp_var) in
        Some ret_val, Cil.mkStmt (Return (Some (Lval ret_val), Cil.locUnknown))
      )
    in
    let callStmFun = Call(ret_lval, Lval(Cil.var stmFun.svar), arguments, Cil.locUnknown) in
    let startTx = Call(None, Lval(var (getSome !startTx_function)), [getSome !stmEnviron], Cil.locUnknown) in
    let commitResult = Cil.var (Cil.makeTempVar f_new ~name:"txResult" Cil.intType) in
    let initComRes = Set(commitResult, Cil.zero, Cil.locUnknown) in
    let commitTx = Call(Some commitResult, Lval(var (getSome !commitTx_function)), [getSome !stmEnviron], Cil.locUnknown) in
    let whileBody = [Cil.mkStmt(Instr [callStmFun;commitTx])] in
    let whileStmtLst = mkWhile ~guard:(UnOp(LNot,Lval commitResult,Cil.intType)) ~body:whileBody in
    (* Locks *)
    let unlock = var (getSome !unlock_function) in
    let acq, rel = Usedef.VS.fold
      (fun l (acq, rel) -> 
        let acql = Call(None, Lval(lock), [Cil.mkAddrOf (Cil.var l)], Cil.locUnknown) in
        let rell = Call(None, Lval(unlock), [Cil.mkAddrOf (Cil.var l)], Cil.locUnknown) in
        (acql::acq, rell::rel)
      )
      locks
      ([], [])
    in

    f_new.sbody.bstmts <- f_new.sbody.bstmts 
      @ [Cil.mkStmt (Instr acq)]
      @ [Cil.mkStmtOneInstr startTx] 
      @ [Cil.mkStmtOneInstr initComRes] 
      @ whileStmtLst 
      @ [Cil.mkStmt (Instr (List.rev rel))]
      @ [ret_stmt];
    f_new
    end

  let make_lock_wrapper (f: fundec) (locks: Usedef.VS.t): fundec = begin
    (* make an empty function *)
    let f_new = Cil.emptyFunction (f.svar.vname ^ "__atomic") in
    (* declare its argument names and types *)
    Cil.setFunctionTypeMakeFormals f_new f.svar.vtype;
    (* create a new local variable with type equal to the return type *)
    let ret_type = get_return_type f.svar.vtype in
    let arguments = List.map (fun v -> Lval(Cil.var v)) f_new.sformals in
    let ret_lval, ret_stmt = 
      if isVoidType ret_type then (
        None, Cil.mkStmt (Return(None, Cil.locUnknown))
      ) else (
        let tmp_var: varinfo = Cil.makeTempVar f_new ~name:"retVal" ret_type in
        let ret_val: lval = (Cil.var tmp_var) in
        Some ret_val, Cil.mkStmt (Return (Some (Lval ret_val), Cil.locUnknown))
      )
    in
    let callFun = Call(ret_lval, Lval(Cil.var f.svar), arguments, Cil.locUnknown) in
    (* Locks *)
    let lock = var (getSome !wr_lock_function) in
    let unlock = var (getSome !unlock_function) in
    let acq, rel = Usedef.VS.fold
      (fun l (acq, rel) -> 
        let acql = Call(None, Lval(lock), [Cil.mkAddrOf (Cil.var l)], Cil.locUnknown) in
        let rell = Call(None, Lval(unlock), [Cil.mkAddrOf (Cil.var l)], Cil.locUnknown) in
        (acql::acq, rell::rel)
      )
      locks
      ([], [])
    in

    f_new.sbody.bstmts <- f_new.sbody.bstmts 
      @ [Cil.mkStmt (Instr acq)]
      @ [Cil.mkStmtOneInstr callFun] 
      @ [Cil.mkStmt (Instr (List.rev rel))]
      @ [ret_stmt];
    f_new
  end

  let isNotLocal (v: varinfo ) slocals = not (List.mem v slocals)

  (* Visitor that re-writes an expr so all non stack local memory reads are wrapped in a call to read_function *)
  class addSTMReadFunVisitor f_new instList = object (self) 
    inherit nopCilVisitor
    method vexpr ex = 
      match ex with
      | Lval(lv) -> begin
          match lv with
          | Var(v),_ when isNotLocal v f_new.slocals ->
              let tmp = makeTempVar f_new (Cil.typeOfLval lv) in
              let readIntoTmp =
                Call(Some (var tmp),Lval(var (getSome !read_function)),
                     [getSome !stmEnviron;
		      Cil.mkCast ~e:(Cil.mkAddrOf lv) ~newt:(getSome !addr_type)],
                     Cil.locUnknown)
              in
              instList := readIntoTmp::!instList;
              ChangeTo (Lval(var tmp))
	  | Mem(exp),_ ->
              let tmp = makeTempVar f_new (Cil.typeOfLval lv) in
	      let v = new addSTMReadFunVisitor f_new instList in
	      let newExp = Cil.visitCilExpr v (Cil.mkAddrOf lv) in
              let readIntoTmp =
                Call(Some (var tmp),Lval(var (getSome !read_function)),
                    [getSome !stmEnviron;
		     Cil.mkCast ~e:newExp ~newt:(getSome !addr_type)],
                    Cil.locUnknown)
              in
	      instList := !instList @ [readIntoTmp];
	      ChangeTo (Lval(var tmp))
          | _ -> DoChildren
        end
      | _ -> DoChildren
  end

  (* extract all the reads needed for this expr to be STM safe *)
  let addStmReads f_new exp = 
    let instList = ref [] in 
    let v = new addSTMReadFunVisitor f_new instList in
    let newExp = Cil.visitCilExpr v exp in
    (!instList,newExp)
    
  let createCallToWrite res value loc =
    Call(None,Lval(var (getSome !write_function)), 
	[getSome !stmEnviron;
	 Cil.mkCast ~e:res ~newt:(getSome !addr_type);
	 Cil.mkCast ~e:value ~newt:(getSome !word_type)],
	loc)

  (* For each function, add reads and writes for expressions found in
   Instructions: Set, Call
   Statements: Return, If, Switch *)
  class modStmFunVisitor (f_new: fundec) (stm_functions: (string,Cil.fundec) Hashtbl.t): cilVisitor =
    object (self)
    inherit nopCilVisitor
    method vinst inst =
      match inst with
      | Set(lv, exp, loc) -> begin
          let (instList,newExp) = addStmReads f_new exp in
          let newSet = match lv with
            | Var(v),_ when isNotLocal v f_new.slocals ->
		[createCallToWrite (Cil.mkAddrOf lv) newExp loc]
	    | Mem(exp2),_ ->
		let instList2,newExp2 = addStmReads f_new exp2 in
		instList2 @ [createCallToWrite newExp2 newExp loc]
            | _ -> [Set(lv,newExp,loc)]
          in
          ChangeTo (instList @ newSet)
        end
      | Call(lvOpt,Lval(Var(fvinfo),off),argList,loc) -> begin
	    let funInstList,newFunExp = (
		let newFD = Hashtbl.find stm_functions fvinfo.vname in
		let exp = Lval(Var(newFD.svar),off) in
		  [],exp
	      ) in
	    let instListList,newArgList = List.split(List.map (addStmReads f_new) argList) in
	    let instList = List.flatten instListList in
	    let retInstList,newLvOpt = match lvOpt with 
	      | None -> [],None
	      | Some(lv) -> match lv with
		  | Var(v),_ when isNotLocal v f_new.slocals ->
		      let tmp = Cil.var (Cil.makeTempVar f_new (Cil.typeOfLval lv)) in
		      let writeCall = createCallToWrite (Cil.mkAddrOf lv) (Lval tmp) loc in
			[writeCall],Some tmp
		  | Mem(exp2),_ ->
		      let instList2,newExp2 = addStmReads f_new exp2 in
		      let tmp = Cil.var (Cil.makeTempVar f_new (Cil.typeOfLval lv)) in
		      let writeCall = createCallToWrite newExp2 (Lval tmp) loc in
			instList2 @ [writeCall], Some tmp
		  | _ -> [],lvOpt
	    in
	    let newCall = Call(newLvOpt,newFunExp,newArgList,loc) in
	      ChangeTo (instList @ funInstList @ [newCall] @ retInstList)
	end
      | _ -> DoChildren

    method vstmt statement =
      match statement.skind with
      | Return(Some exp,loc) -> begin
          let (instList,newExp) = addStmReads f_new exp in
          let reads = Cil.mkStmt (Instr instList) in
          let ret = Cil.mkStmt (Return (Some newExp,loc)) in
          ChangeTo (Cil.mkStmt(Block (Cil.mkBlock [reads;ret])))
        end
      | If(exp,b1,b2,loc) -> begin
          let (instList,newExp) = addStmReads f_new exp in
          let reads = Cil.mkStmt (Instr instList) in
          let v = new modStmFunVisitor f_new stm_functions in
          let newb1 = visitCilBlock v b1 in
          let newb2 = visitCilBlock v b2 in
          let newIf = Cil.mkStmt (If (newExp,newb1,newb2,loc)) in
          ChangeTo(Cil.mkStmt(Block (Cil.mkBlock [reads;newIf])))
	end
      | Switch(exp,blk,slist,loc) -> begin
	  ignore(E.log("ERROR, switch statements not supported in STM mode\n"));
	  SkipChildren
	  (*
	  let (instList,newExp) = addStmReads f_new exp in
          let reads = Cil.mkStmt (Instr instList) in
          let v = new modStmFunVisitor f_new stm_functions in
          let newBlk = visitCilBlock v blk in
          let newsList = List.map (visitCilStmt v) slist in
          let newSw = Cil.mkStmt (Switch (newExp,newBlk,newsList,loc)) in
          ChangeTo(Cil.mkStmt(Block (Cil.mkBlock [reads;newSw])))
	  *)
        end
      | _ -> DoChildren
  end

  let rewrite_stm_function (f: file) (stm_functions) _ (fd:fundec) : unit = begin
      let v = new modStmFunVisitor fd stm_functions in
      ignore(visitCilFunction v fd)
    end

  let make_thread_spawn_wrapper (fd:Cil.fundec) = begin
     (* make an empty function *)
    let fd_new = Cil.emptyFunction (fd.svar.vname ^ "__thread_spawn") in
    (* declare its argument names and types *)
    Cil.setFunctionTypeMakeFormals fd_new fd.svar.vtype;
    (* create a new local variable with type equal to the return type *)
    let ret_type = get_return_type fd.svar.vtype in
    let arguments = List.map (fun v -> Lval(Cil.var v)) fd_new.sformals in
    let ret_lval, ret_stmt = 
      if isVoidType ret_type then (
        None, Cil.mkStmt (Return(None, Cil.locUnknown))
      ) else (
        let tmp_var: varinfo = Cil.makeTempVar fd_new ~name:"retVal" ret_type in
        let ret_val: lval = (Cil.var tmp_var) in
        Some ret_val, Cil.mkStmt (Return (Some (Lval ret_val), Cil.locUnknown))
      )
    in
    let initThread = Call(None, Lval(Cil.var (getSome !addThread_function)),[getSome !stmEnviron] , Cil.locUnknown) in
    let callOldFun = Call(ret_lval, Lval(Cil.var fd.svar), arguments, Cil.locUnknown) in
    let removeThread = Call(None, Lval(Cil.var (getSome !removeThread_function)),[getSome !stmEnviron] , Cil.locUnknown) in
    let newBody = Instr [initThread;callOldFun;removeThread] in

    fd_new.sbody.bstmts <- fd_new.sbody.bstmts 
      @ [Cil.mkStmt newBody]
      @ [ret_stmt];
    fd_new
    end
    

  class modThreadSpawnVisitor (f:file) (ts_functions: (string,Cil.fundec) Hashtbl.t): cilVisitor =
    object (self)
    inherit nopCilVisitor
    method vinst inst =
      match inst with
	| Call(lvOpt,Lval(Var(fvinfo),off),(thread::attr::start_routine::arg::[]),loc) when fvinfo.vname = "pthread_create" -> begin
	      (match start_routine with 
		| AddrOf(Var(ts_info),_) -> begin
		    let new_ts_fd = if not(Hashtbl.mem ts_functions ts_info.vname) then (
		      let fd = find_function_fundec f ts_info.vname in
		      let new_fd = make_thread_spawn_wrapper fd in
		      Hashtbl.add ts_functions ts_info.vname new_fd;
		      new_fd) 
		      else( Hashtbl.find ts_functions ts_info.vname) in
		    let new_ts_var = AddrOf(Cil.var new_ts_fd.svar) in
		    let new_call = Call(lvOpt,Lval(Var(fvinfo),off),thread::attr::new_ts_var::arg::[],loc) in
		    ChangeTo [new_call]
		  end
		| _ -> raise Not_found)
	  end
	| _ -> DoChildren
  end

  let rewrite_thread_spawn (f: file) : unit = begin
      let tsFuns = Hashtbl.create 1 in
      let v = new modThreadSpawnVisitor f tsFuns in(
      try (visitCilFile v f) 
      with Not_found ->	ignore(E.log "ERROR, arg3 of pthread_create must by in the form &localFunction"));
      Hashtbl.iter (fun name new_fd -> add_after f (find_function_fundec f name) new_fd) tsFuns
  end

    
  let make_stm_atomics (f: file) (stm_functions) (fun_to_locks) (fd: fundec) _ : unit =
    begin
    let locks = if Hashtbl.mem fun_to_locks fd then Hashtbl.find fun_to_locks fd 
      else Usedef.VS.empty
    in
    let stm = Hashtbl.find stm_functions fd.svar.vname in
    let mw = make_stm_wrapper fd stm locks (Cil.var (getSome !rd_lock_function)) in
    change_var f fd.svar mw.svar;
    add_after f fd mw;
  end

  let make_non_stm_atomics (f: file) (fun_to_locks) (fd: fundec) _ : unit =
    begin
    let locks = if Hashtbl.mem fun_to_locks fd then Hashtbl.find fun_to_locks fd 
      else Usedef.VS.empty
    in
    let mw = make_lock_wrapper fd locks in
    change_var f fd.svar mw.svar;
    add_after f fd mw;
  end


end

let is_atomic (v: varinfo) : bool =
  Cil.hasAttribute "atomic" v.vattr
class findAtomicFunVisitor aflist : cilVisitor = object (self)
  inherit nopCilVisitor
  method vglob glob =
    match glob with
      | GFun(fd, _) ->
          if is_atomic fd.svar then (
            assert (not (Hashtbl.mem aflist fd));
            Hashtbl.add aflist fd ();
          );
          SkipChildren
      | _ -> SkipChildren
  end
let find_atomic_functions f =
  let atomic_functions : (Cil.fundec, unit) Hashtbl.t = Hashtbl.create 1 in
  let v = new findAtomicFunVisitor atomic_functions in
  Cil.visitCilFileSameGlobals v f;
  atomic_functions

(* For each function, duplicate it & add it after the original *)
let create_stm_functions needed_stm_functions (f:file) = 
  let stm_functions = Hashtbl.create 1 in
  let dupe fd _ = 
    let f_new = Cil.copyFunction fd (fd.svar.vname ^ "__stm") in
    add_after f fd f_new;
    Hashtbl.add stm_functions fd.svar.vname f_new 
  in
    Hashtbl.iter dupe needed_stm_functions;
    stm_functions

(* Visitor that locates the functions called by the current function *)
class findStmFunVisitor (f: file) (stmTbl) (badFuns) : cilVisitor = object (self)
  inherit nopCilVisitor
  method vinst inst =
    (match inst with
	| Call(_,Lval(Var(svar),_),_,_) -> begin
	    try 
	      let fd = find_function_fundec f svar.vname in
		if not (Hashtbl.mem stmTbl fd) then (
		    Hashtbl.add stmTbl fd ();)
	    with Not_found -> 
	      if not (Hashtbl.mem badFuns svar.vname) then (
		  Hashtbl.add badFuns svar.vname ();)
	  end
	| Call(_,_,_,_) ->
	    ignore(E.log "ERROR, Function call of a non Lval(Var)\n")
	| _ -> ());
      SkipChildren
end

(* Put the elements from src into dest if they don't already exist *)
let mergeInto dstTbl srcTbl =
  let changed = ref false in
  let addInto k v = 
    if not (Hashtbl.mem dstTbl k) then(
	Hashtbl.add dstTbl k v;
	changed := true;)
  in
  Hashtbl.iter addInto srcTbl;
  !changed

(* Recursive function that given some functions, finds all the functions they call all the way down.
  If a defintion cannot be found, it is added to the bad_functions Hash*)
let rec find_stmFuns stm_functions bad_functions (f:file) =
  let changed = ref false in
  let findNew fd _ =
    let new_functions = Hashtbl.create 1 in
    let v = new findStmFunVisitor f new_functions bad_functions in
    ignore(Cil.visitCilFunction v fd);
    if (mergeInto stm_functions new_functions) then (changed := true)
  in
  Hashtbl.iter findNew stm_functions;
  if !changed then
    find_stmFuns stm_functions bad_functions f

(* For each atomic function, find all the stm functions needed for it,
   If any bad functions are found, inform the user.
   After all the needed stm functions are found, duplicate them*)
let find_stm_functions atomic_functions (f:file) =
  let stm_functions = Hashtbl.create 1 in
  let foundBadFun = ref false in
  let badFuns = Hashtbl.create 1 in
  let goodFuns = Hashtbl.create 1 in
  let findNew fd _ = 
    let new_functions = Hashtbl.create 1 in
    let bad_functions = Hashtbl.create 1 in
    Hashtbl.add new_functions fd ();
    find_stmFuns new_functions bad_functions f;
    if Hashtbl.length bad_functions == 0 then(
      Hashtbl.add goodFuns fd ();
      ignore(mergeInto stm_functions new_functions)
    )else(
      foundBadFun := true;
      Hashtbl.add badFuns fd ();
      ignore(E.log "WARNING: Atomic function '%s' calls functions that are not defined locally\n" fd.svar.vname );
      ignore(E.log "Inferred locks will be used to enforce atomicity\n");
      let logStr s _ =
	ignore(E.log "\t%s\n" s)
      in Hashtbl.iter logStr bad_functions
      )
  in
  Hashtbl.iter findNew atomic_functions;
  if !debug then(
      ignore(E.log "STMed Functions: \n");
      let logFun fd _ = 
	ignore(E.log "\t%s()\n" fd.svar.vname) 
      in Hashtbl.iter logFun stm_functions;);
  (stm_functions,goodFuns,badFuns)


let doit (f: file) : unit = begin
  Rmtmps.removeUnusedTemps f;
  Rmalias.removeAliasAttr f;
  preprocessAndMergeWith f !pthread_h_name;
  preprocessAndMergeWith f !stm_h_name;
  let atomic_functions = find_atomic_functions f in
  let (needed_stm_functions,stm_atomics,non_stm_atomics) = find_stm_functions atomic_functions f in

  STMRewriter.init f;

  let fun_to_locks = Lockpick.infer_locks f (STMRewriter.get_lock_type ()) (STMRewriter.get_init_fun ()) stm_atomics in
  let stm_functions = create_stm_functions needed_stm_functions f in

  (* Create the global init function after calling lockpick or the 
     Liveness analysis will throw an exception on it *)
  STMRewriter.initGlobInit f;

  (* Re-write the stm functions *)
  Hashtbl.iter 
    (STMRewriter.rewrite_stm_function f stm_functions)
    stm_functions;

  (* Re-write the atomic stm functions *)
  Hashtbl.iter
    (STMRewriter.make_stm_atomics f stm_functions fun_to_locks)
    stm_atomics;

  (* Re-write the atomic non-stm functions *)
  Hashtbl.iter
    (STMRewriter.make_non_stm_atomics f fun_to_locks)
    non_stm_atomics;

  (* Re-write the thread-spawn functions *)
  STMRewriter.rewrite_thread_spawn f;

  Rmtmps.removeUnusedTemps f;

end

let feature : featureDescr = {
  fd_name = "stm";
  fd_enabled = ref false;
  fd_description = "stm";
  fd_extraopt = options;
  fd_doit = doit;
  fd_post_check = true;
}
