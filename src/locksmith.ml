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
open Lockutil
open Cil
module Lprof = Lockprofile

(*****************************************************************************)

let do_graph_out = ref false
let do_cfgraph_out = ref false
let listphase = ref false

(* subversion will substitute this *)
let version = "$Rev: 5624 $"

let print_version () =
  print_endline ("LockSmith version: " ^ version); 
  exit 0


let feature : featureDescr = {
  fd_name = "locksmith";
  fd_enabled = ref true;
  fd_description = "Locksmith";

  fd_extraopt =
    [
      "--locksmith-version",
        Arg.Unit(print_version),
        " Print locksmith version and exit.";

      "--debug-locksmith",
        Arg.Set(listphase),
        " Print high-level progress info.";

      "--save-graph",
        Arg.Set(do_graph_out),
        " Write constraints in \"graph.dot\".";

      "--save-control-flow-graph",
        Arg.Set(do_cfgraph_out),
        " Write constraints in \"cf-graph.dot\".";
    ]
    @ Locktype.options
    @ Uniqueness.options
    @ Semiunification.options
    @ Locksettings.options
    (*@ Livevars.options*)
    @ Shared.options
    @ Lockstate.options
    @ Correlation.options
    @ Controlflow.options
    @ Bansheemlifc.options
    @ Labelflow.options
    @ Lprof.options
    ;
  fd_doit =
  (function (f: file) -> begin
    Rmtmps.removeUnusedTemps f;
    Rmalias.removeAliasAttr f;
    ignore(E.log
      "\n************************* STARTING *************************\n\n");
    ignore (Sys.signal Sys.sigusr1 (Sys.Signal_handle (fun (i: int) -> Lprof.endtime "killed"; exit 1)));
    Lprof.endtime "start";
    if !listphase then ignore(E.log "phase-begin(typing)\n");
    Locktype.generate_constraints f;
    Labelflow.done_adding ();
    Lprof.endtime "typing";
    if !listphase then ignore(E.log "phase-end(typing)\n");

    if !do_cfgraph_out then begin
      Dotpretty.init_file "cf-graph.dot" "control flow graph";
      Lockstate.print_graph !Dotpretty.outf;
      Dotpretty.close_file ();
    end;

    if !do_graph_out then begin
      Dotpretty.init_file "graph-begin.dot" "initial constraints";
      Labelflow.print_graph !Dotpretty.outf;
      Semiunification.print_graph !Dotpretty.outf;
      Lockstate.print_graph !Dotpretty.outf;
      Dotpretty.close_file ();
    end;

    if !listphase then ignore(E.log "phase-begin(shared)\n");
    Shared.solve (Locktype.get_global_var_rhos ());
    Lprof.endtime "shared";
    if !listphase then ignore(E.log "phase-end(shared)\n");

    if !listphase then ignore(E.log "phase-begin(linearity)\n");
    Semiunification.solve ();
    Lprof.endtime "linearity";
    if !listphase then ignore(E.log "phase-end(linearity)\n");

    if !listphase then ignore(E.log "phase-begin(lock-state)\n");
    Lockstate.solve ();
    Lprof.endtime "lock-state";
    if !listphase then ignore(E.log "phase-end(lock-state)\n");

    if !listphase then ignore(E.log "phase-begin(guarded-by)\n");
    Correlation.solve ();
    Lprof.endtime "guarded-by";
    if !listphase then ignore(E.log "phase-end(guarded-by)\n");

    if !listphase then ignore(E.log "phase-begin(escapes)\n");
    Semiunification.check ();
    Lprof.endtime "escapes";
    if !listphase then ignore(E.log "phase-end(escapes)\n");

    if !listphase then ignore(E.log "phase-begin(races)\n");
    Correlation.check_races ();
    Lprof.endtime "races";
    if !do_graph_out then begin
      Dotpretty.init_file "graph.dot" "solved constraints";
      Labelflow.print_graph !Dotpretty.outf;
      Semiunification.print_graph !Dotpretty.outf;
      Lockstate.print_graph !Dotpretty.outf;
      Dotpretty.close_file ();
    end;
    if !listphase then ignore(E.log "phase-end(races)\n");

    (*Labelflow.dump_all_chi ();*)
    Lprof.print_stats();
    ignore(E.log
      "*************************** DONE ***************************\n\n");
    ignore(E.log "LockSmith run for: %f seconds\n\n" (Sys.time()));
  end);
  fd_post_check = true;
}
