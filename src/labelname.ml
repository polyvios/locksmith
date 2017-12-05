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
module U = Uref
module E = Errormsg

type label_name =
  | Const      of string
  | AddrOf     of label_name
(*  | List       of label_name list U.uref*)
  | Field      of label_name * string
  | Deref      of label_name

let rec compare l1 l2 =
  match l1, l2 with
    Const s1, Const s2 -> String.compare s1 s2
  | AddrOf l1', AddrOf l2'
  | Deref l1', Deref l2' ->
      compare l1' l2'
  | Field(l1', f1), Field (l2', f2) ->
      let x = compare l1' l2' in
      if x = 0 then String.compare f1 f2
      else x
  | Const _, _
  | AddrOf _, Deref _
  | AddrOf _, Field _
  | Deref _, Field _ -> -1
  | _, _ -> 1

let rec d_label_name_r known () l =
  if List.memq l known then nil else
  let known = l::known in
  let tmp =
  match l with 
  | Const s -> 
      text s
  | AddrOf l ->
      dprintf "&%a" (d_label_name_r known) l
  (*| List llu ->
      let ll = U.deref llu in
      dprintf "(%a)" (d_list ", " (d_label_name_r known)) ll*)
  | Field (Deref l,f) ->
      dprintf "%a->%s" (d_label_name_r known) l f
  | Field (l, f) ->
      dprintf "%a.%s" (d_label_name_r known) l f
  | Deref l ->
      dprintf "*%a" (d_label_name_r known) l
  in
  tmp
let d_label_name () l = d_label_name_r [] () l

let string_of_label_name l : string =
  Pretty.sprint 800 (d_label_name () l)
