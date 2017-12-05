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
module Strset : Set.S with type elt = string
module Strmap : Map.S with type key = string
module StrHT : Hashtbl.S with type key = string
module type HashedOrderedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
    val compare : t -> t -> int
  end
  
val isSome : 'a option -> bool
val getSome : 'a option -> 'a

(* Get the return type of a function type.
 * The argument must be Cil.typ of kind TFun.
 *)
val get_return_type : Cil.typ -> Cil.typ

(* find the (first) typedef for type "name" in file f *)
val find_type : Cil.file -> string -> Cil.typ

(* find the (first) declaration of variable "name" in file f *)
val find_function_var : Cil.file -> string -> Cil.varinfo

(* find the function definition of variable "name" in file f, throws Not_found on error *)
val find_function_fundec : Cil.file -> string -> Cil.fundec

(* Adds a declaration of function f_new right AFTER the first occurence
 * (declaration or definition) of function f_old.  Also, the body of the
 * function f_new is added at the end of the file.
 *)
val add_after : Cil.file -> Cil.fundec -> Cil.fundec -> unit

(* Changes all call sites of function f_old, to rather call function f_new
 * instead.  f_new and f_old must have the same signature.
 *)
val change_var : Cil.file -> Cil.varinfo -> Cil.varinfo -> unit

(* take a cil file, and a string pointing to a file, preprocess it and merge
 * with the first.  Merging modifies the first argument.
 *) 
val preprocessAndMergeWith : Cil.file -> string -> unit
