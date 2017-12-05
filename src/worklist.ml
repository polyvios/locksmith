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
let debug = ref false

module type S =
  sig
    type elt
    type t
    exception Empty
    val create : unit -> t
    val clear : t -> unit
    val push : elt -> t -> unit
    val pop : t -> elt
    val is_empty : t -> bool
    val length : t -> int
  end

module UniqueWorklist =
  functor (W: S) ->
  functor (H: Hashtbl.S with type key = W.elt) ->
  struct
    type elt = W.elt
    type t = {
      h: unit H.t; (* membership hash *)
      w: W.t; (* actual worklist *)
    }
    exception Empty = W.Empty

    let create () : t = {
      h = H.create 100;
      w = W.create ();
    }

    let clear w =
      H.clear w.h;
      W.clear w.w

    let push (p: elt) (worklist: t) : unit =
      if H.mem worklist.h p then ()
      else W.push p worklist.w

    let pop (worklist: t) : elt =
      let p = W.pop worklist.w in
      H.remove worklist.h p;
      p
    let is_empty w = W.is_empty w.w
    let length w = W.length w.w
  end

module type Enumerable =
  sig
    type t
    val order : t -> int
  end

module HeapWorklist =
  functor (N: Enumerable) ->
  struct
    type elt = N.t
    type t = elt Heap.t
    exception Empty
    let create () : t = Heap.create 100
    let clear = Heap.clear
    let push (x: elt) (worklist: elt Heap.t) : unit =
      Heap.insert worklist (N.order x) x

    let pop (worklist: t) : elt =
      if Heap.is_empty worklist then raise Empty
      else snd (Heap.extract_max worklist)

    let is_empty = Heap.is_empty
    let length = Heap.length
  end

module SetWorklist (Ord: Set.OrderedType) =
  struct
    module S = Set.Make(Ord)
    type elt = Ord.t
    type t = S.t ref
    exception Empty
    let create () : t = ref S.empty
    let clear w = w := S.empty

    let push (p: elt) (w: t) : unit = w := S.add p !w

    let pop (w: t) : elt =
      if S.is_empty !w then raise Empty
      else
        let p = S.max_elt !w in
        w := S.remove p !w;
        p

    let is_empty w = S.is_empty !w
    let length w = S.cardinal !w
  end

module StackWorklist =
 struct
    type 'a t = 'a list ref
    exception Empty
    let create () = ref []
    let clear s = s := []
    let is_empty s = !s = []
    let push x s = s := x::!s
    let pop s =
      match !s with
        [] -> raise Empty
      | h::tl -> s := tl; h
    let length w = List.length !w
  end

module DoubleStackWorklist =
 struct
    type 'a t = {
      mutable s1: 'a list;
      mutable s2: 'a list;
    }
    exception Empty
    let create () = { s1 = []; s2 = []; }
    let clear s = s.s1 <- []; s.s2 <- []
    let is_empty s = (s.s1 = [] && s.s2 = [])
    let push p s = s.s2 <- p::s.s2
    let pop s =
      if is_empty s then raise Empty else
      match s.s1 with
        [] -> (* swap *)
          let p = List.hd s.s2 in
          s.s1 <- (List.tl s.s2);
          s.s2 <- [];
          p
      | h::tl ->
          s.s1 <- tl;
          h
    let length w = (List.length w.s1) + (List.length w.s2)
  end

module QueueWorklist = Queue
 (*struct
    type 'a t = 'a Queue.t
    exception Empty = Queue.Empty
    let create = Queue.create
    let clear = Queue.clear
    let is_empty = Queue.is_empty
    let push = Queue.push
    let pop = Queue.pop
    let length = Queue.length
  end*)

(* this one is not a worklist.  It's just a boolean that stores whether something has changed.
 * THIS IS ONLY TO BE USED WITH --only-postorder.
 *)
module BoolWorklist =
  struct
    type 'a t = bool ref
    exception Empty
    let create () = ref false
    let clear w = w := false
    let is_empty w = not !w
    let push p w = w := true
    let pop w = raise Empty
    let length w = if !w then 1 else 0
  end
