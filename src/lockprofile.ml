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
external get_mem_usage: unit -> int = "get_usage"
external get_mem_usage2: unit -> int = "get_usage2"
external get_banshee_mem: unit -> int = "get_profile_mem"
module E = Errormsg

let print_mem_usage = ref false
let save_mem_usage = ref false
let mem_usage_file = ref stdout
let stop_after = ref ""

let options = [
  "--profile-locksmith",
    Arg.Set(print_mem_usage),
    " Print time and memory usage for each phase.";

  "--save-profile-data",
    Arg.String(fun s ->
       save_mem_usage := true;
       mem_usage_file := open_out s),
    " Takes 1 argument (filename). Write profiling information to the specified file.";

  "--stop-after",
    Arg.String(fun s -> stop_after := s),
    " Stop after the given phase."
]

type t = float

let get_mem_info () : string = begin
    let s: Gc.stat = Gc.stat () in
    let ocamlmem = (s.Gc.live_words * 4) / 1048576 in
    let kernelmem = (get_mem_usage ()) / 1048576 in
    (*let kernelmem2 = (get_mem_usage2 ()) / 1048576 in*)
    let bansheemem = (get_banshee_mem ()) / 1048576 in
    Printf.sprintf "%s %d %d %d"
      (Labelflow.get_stats ()) kernelmem (*kernelmem2*) bansheemem ocamlmem
end

let starttime = Sys.time ()
let endtimes = ref [] (*("cil-start", starttime, get_mem_info ())*)

let string_of_time (r: float) : string =
  let t = int_of_float (r *. 100.0) in
  let point = t mod 100 in
  let t = t / 100 in
  let s = t mod 60 in
  let m = (t mod 3600) / 60 in
  let h = t / 3600 in
  Printf.sprintf "%02d:%02d:%02d.%02d" h m s point

let endtime (phase: string) : unit = begin
  let mem =  get_mem_info () in
  let t = Sys.time () in
  endtimes := (phase, t, mem)::!endtimes;
  if !print_mem_usage then
    ignore(E.log "profile-data(%s) %s %s\n" phase (string_of_time t) mem);
  if !save_mem_usage then begin
    Printf.fprintf !mem_usage_file "%s %s %f %s\n" phase (string_of_time t) t mem;
    flush !mem_usage_file;
  end;
  if !stop_after = phase then exit 0;
end

let last_timestamp = ref 0

let abs_time_to_string t =
  let s = t mod 60 in
  let m = (t mod 3600) / 60 in
  let h = t / 3600 in
  Printf.sprintf "%02d:%02d:%02d" h m s

let timestamp () =
  let t = int_of_float (Sys.time ()) in
  let d = t - (!last_timestamp) in
  begin
    last_timestamp := t;
    "(" ^ (abs_time_to_string t) ^ " [+" ^ (abs_time_to_string d) ^ "])"
  end

let print_stats () : unit = begin
  if !save_mem_usage then begin
    let outf = !mem_usage_file in
    (*List.iter (fun (s,t,mem) ->
      Printf.fprintf outf "%s %s %f %s\n" s (string_of_time t) t mem;
    ) (List.rev !endtimes);*)
    close_out outf;
    mem_usage_file := stdout;
  end
end
