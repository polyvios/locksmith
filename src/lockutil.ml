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
open Cil

module Strset = Set.Make(String)
module Strmap = Map.Make(String)
module StrHT = Hashtbl.Make(
  struct
    type t = string
    let equal s1 s2 = s1=s2
    let hash s =
      let len = String.length s in
      let loop (h:int) (i:int) =
        if i = len then h
        else h*33 + (int_of_char (s.[i])) in
      loop 5381 0
        (* Hash function djb2 from http://www.cs.yorku.ca/~oz/hash.html *)
  end)

module type HashedOrderedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end

let isSome  = function
    None -> false
  | Some _ -> true

let getSome = function
    None -> raise (Failure "getSome")
  | Some x -> x

(*****************************************************************************)

let debug = ref false

(* Get the return type of a function type.
 * The argument must be Cil.typ of kind TFun.
 *)
let get_return_type = function
  | TFun(r, _, _, _) -> r
  | _ -> assert false

(* find the (first) typedef for type "name" in file f *)
exception Found_type of typ
let find_type (f: file) (name: string) : typ =
  let findit = function
    | GType(ti, _) when ti.tname = name -> raise (Found_type (TNamed(ti, [])))
    | _ -> ()
  in
  try
    Cil.iterGlobals f findit;
    raise Not_found
  with Found_type t -> t

(* find the (first) declaration of variable "name" in file f *)
exception Found_varinfo of varinfo
let find_function_var (f: file) (name: string) : varinfo =
  let findit = function
    | GVarDecl(vi, _) when vi.vname = name -> raise (Found_varinfo vi)
    | _ -> ()
  in
  try
    Cil.iterGlobals f findit;
    raise Not_found
  with Found_varinfo v -> v

(* find the function definition of variable "name" in file f *)
exception Found_fundec of fundec
let find_function_fundec (f: file) (name: string) : fundec =
  let findit = function
    | GFun(fd, _) when fd.svar.vname = name -> raise (Found_fundec fd)
    | _ -> ()
  in
  try
    Cil.iterGlobals f findit;
    raise Not_found
  with Found_fundec v -> v


class addFunVisitor (f_old: string) (f_new: fundec) : cilVisitor = object (self)
  inherit nopCilVisitor
  method vglob glob =
    match glob with
      | GFun(fd, _) when fd.svar.vname = f_old ->
          ChangeTo [glob; GVarDecl(f_new.svar, Cil.locUnknown)]
      | GVarDecl(vi, _) when vi.vname = f_old ->
          ChangeTo [glob; GVarDecl(f_new.svar, Cil.locUnknown)]
      | _ -> SkipChildren
  end

(* Adds a declaration of function f_new right AFTER the first occurence
 * (declaration or definition) of function f_old.  Also, the body of the
 * function f_new is added at the end of the file.
 *)
let add_after (file: Cil.file) (f_old: fundec) (f_new: fundec) : unit =
  let v = new addFunVisitor f_old.svar.vname f_new in
  visitCilFile v file;
  file.globals <- file.globals @ [GFun(f_new, Cil.locUnknown)]

class changeVarinfoVisitor (oldv: varinfo) (newv: varinfo) : cilVisitor =
  object (self)
    inherit nopCilVisitor
    method vvrbl v = 
      if v.vid = oldv.vid then ChangeTo newv else SkipChildren
  end

(* Changes all call sites of function f_old, to rather call function f_new
 * instead.  f_new and f_old must have the same signature.
 *)
let change_var (file: Cil.file) (oldv: varinfo) (newv: varinfo) : unit =
  let v = new changeVarinfoVisitor oldv newv in
  visitCilFileSameGlobals v file

let rec isNotLocal (v: varinfo) slocals = 
  not (List.mem v slocals)

let preprocessAndMergeWith (f: file) (add: string) : unit = begin
  Sys.command ("gcc -E "^(add)^">/tmp/_cil_rewritten_tmp.h");
  let add_h = Frontc.parse "/tmp/_cil_rewritten_tmp.h" () in
  let f' = Mergecil.merge [add_h; f] "stdout" in
  f.globals <- f'.globals;
end
