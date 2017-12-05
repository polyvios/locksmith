(*
 *
 * Copyright (c) 2004-2007, 
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

(* remove declarations of the form:
 * foo_t foo __attribute__((weak, alias("bar")));
 * by replacing all occurences of variable foo with variable bar
 *)

open Cil
open Pretty

module E = Errormsg
module H = Hashtbl

class rmAliasVisitor names_hash replace_hash : cilVisitor = object (self)
  inherit nopCilVisitor
  method vglob = function
    GVarDecl(v, _)
  | GVar(v, _, _) -> (
      if not (H.mem names_hash v.vname) then H.add names_hash v.vname v;
      match filterAttributes "alias" v.vattr with
        [] -> SkipChildren  (* ordinary prototype. *)
      | [Attr("alias", [AStr othername])] ->
          H.add replace_hash v (H.find names_hash othername);
          ChangeTo [] (* remove this declaration *)
      | _ ->
          E.s (error "Bad alias attribute at %a" d_loc !currentLoc);
          SkipChildren
    )
  | _ -> DoChildren
  method vvrbl v =
    try ChangeTo (H.find replace_hash v)
    with Not_found -> SkipChildren
end

let removeAliasAttr = visitCilFile (new rmAliasVisitor (H.create 1) (H.create 1))

(* should be disabled by default, debug use only: *)
let feature : featureDescr =
  { fd_name = "rmalias";
    fd_enabled = ref false;
    fd_description = "remove \"alias\" attribute";
    fd_extraopt = [];
    fd_doit = removeAliasAttr;
    fd_post_check = false;
  }
 
