(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module Delay = Xapi_stdext_threads.Threadext.Delay

let test_wait () =
  let m = Delay.make () in
  let c = Mtime_clock.counter () in
  let expected = Mtime.Span.(5 * s) in
  let max_error = Mtime.Span.(10 * ms) in
  let _ = Delay.wait m 5.0 in
  let wait_time = Mtime_clock.count c in
  let diff = Mtime.Span.abs_diff expected wait_time in
  let cmp = Mtime.Span.compare diff max_error in
  Alcotest.(check bool) "diff is smaller than max error" true (cmp < 0)


let () =
Alcotest.run "Threadext" [("wait", [("wait", `Quick, test_wait)])]