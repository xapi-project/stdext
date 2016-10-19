(*
 * Copyright (C) Citrix Systems Inc.
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

type semaphore = {
  mutable n : int;
  m : Mutex.t;
  c : Condition.t;
}

let create n =
  if n <= 0 then
    failwith (Printf.sprintf 
                "Negative semaphore count value (%d) on semaphore creation" n);
  let m = Mutex.create ()
  and c = Condition.create () in
  { n; m; c; }

let acquire s k =
  if k <= 0 then
    failwith (Printf.sprintf "Negative weight (%d) on semaphore acquisition" k);
  Mutex.lock s.m;
  while s.n < k do
    Condition.wait s.c s.m;
  done;
  if not (s.n >= k) then
    failwith (Printf.sprintf 
                "Invalid semaphore count (%d) after acquisition for (%d)" s.n k);
  s.n <- s.n - k;
  Condition.signal s.c;
  Mutex.unlock s.m

let release s k =
  if k <= 0 then
    failwith (Printf.sprintf "Negative weight (%d) on semaphore release" k);
  Mutex.lock s.m;
  s.n <- s.n + k;
  Condition.signal s.c;
  Mutex.unlock s.m

let execute_with_weight s k f =
  acquire s k;
  try
    let x = f () in
    release s k;
    x
  with e ->
    release s k;
    raise e

let execute s f =
  execute_with_weight s 1 f
