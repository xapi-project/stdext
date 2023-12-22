(*
 * Copyright (C) 2023 Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Xapi_fdcaps
open Operations
open Observations

type t = {
    size: int
  ; delay_read: Delay.t option
  ; delay_write: Delay.t option
  ; kind: Unix.file_kind
}

let make ~size ~delay_read ~delay_write kind =
  {size; delay_read; delay_write; kind}

open QCheck2

let file_kind =
  ( Gen.oneofa Unix.[|S_BLK; S_CHR; S_DIR; S_FIFO; S_LNK; S_REG; S_SOCK|]
  , Print.contramap (Fmt.to_to_string Safefd.pp_kind) Print.string
  )

(* also coincidentally the pipe buffer size on Linux *)
let ocaml_unix_buffer_size = 65536

let sizes =
  Gen.oneofa
    [|
       0
     ; 1
     ; 100
     ; 4096
     ; ocaml_unix_buffer_size - 1
     ; ocaml_unix_buffer_size
     ; ocaml_unix_buffer_size + 1
     ; 2 * ocaml_unix_buffer_size
     ; (10 * ocaml_unix_buffer_size) + 3
    |]

(* some may exceed length of test, but that is what the timeout is for *)
let delays = Gen.oneofa [|0.001; 0.01; 0.1; 1.0|]

let span_of_s s = s *. 1e9 |> Mtime.Span.of_float_ns |> Option.get

let delays =
  let build duration every_bytes = Delay.v ~duration ~every_bytes in
  (* order matters here for shrinking: shrink timeout first so that shrinking completes sooner! *)
  Gen.(map2 build (map span_of_s delays) (1 -- 128000))

(* keep these short *)
let timeouts = Gen.oneofa [|0.0; 0.001; 0.1; 0.3|]

let t =
  let build (delay_read, delay_write, size, kind) =
    make ~delay_read ~delay_write ~size kind
  in
  Gen.(map build @@ tup4 (option delays) (option delays) sizes (fst file_kind))

let print =
  Fmt.to_to_string
  @@ Fmt.(
       record
         [
           field "delay_read" (fun t -> t.delay_read) (option Delay.pp)
         ; field "delay_write" (fun t -> t.delay_write) (option Delay.pp)
         ; field "size" (fun t -> t.size) int
         ; field "file_kind" (fun t -> (snd file_kind) t.kind) string
         ]
     )

let run_ro t data ~f =
  if Option.is_some t.delay_read then
    QCheck2.assume_fail () ;
  (* we can only implement delays on write, skip *)
  CancellableSleep.with_ @@ fun cancel ->
  let finally () = CancellableSleep.cancel cancel in
  let f arg = Fun.protect ~finally (fun () -> f arg) in
  let write =
    match t.delay_write with
    | Some delay ->
        Delay.apply_write cancel delay single_write_substring
    | None ->
        single_write_substring
  in
  observe_ro write ~f t.kind data

let run_wo t ~f =
  if Option.is_some t.delay_write then
    QCheck2.assume_fail () ;
  (* we can only implement delays on write, skip *)
  CancellableSleep.with_ @@ fun cancel ->
  let finally () = CancellableSleep.cancel cancel in
  let f arg = Fun.protect ~finally (fun () -> f arg) in
  let read =
    match t.delay_read with
    | Some delay ->
        Delay.apply_read cancel delay read
    | None ->
        read
  in
  observe_wo read ~f t.kind
