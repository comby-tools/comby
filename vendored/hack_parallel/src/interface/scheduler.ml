(** Copyright (c) 2016-present, Facebook, Inc.
    Modified work Copyright (c) 2018-2019 Rijnard van Tonder
    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Hack_parallel_intf.Std

module Daemon = Daemon


type t = {
  workers: Worker.t list;
  number_of_workers: int;
  bucket_multiplier: int;
}


let entry =
  Worker.register_entry_point ~restore:(fun _ -> ())


let create
    ?(number_of_workers = 1)
    ?(bucket_multiplier = 10)
    () =
  let heap_handle = Memory.get_heap_handle () in
  let workers =
    Hack_parallel_intf.Std.Worker.make
      ~saved_state:()
      ~entry
      ~nbr_procs:number_of_workers
      ~heap_handle
      ~gc_control:Memory.worker_garbage_control
  in
  Memory.connect heap_handle;
  { workers; number_of_workers; bucket_multiplier }


let map_reduce
    { workers; number_of_workers; bucket_multiplier }
    ?bucket_size
    ~init
    ~map
    ~reduce
    work =
  let number_of_workers =
    match bucket_size with
    | Some exact_size when exact_size > 0 ->
      (List.length work / exact_size) + 1
    | _ ->
      let bucket_multiplier = Core_kernel.Int.min bucket_multiplier (1 + (List.length work / 400)) in
      number_of_workers * bucket_multiplier
  in
  MultiWorker.call
    (Some workers)
    ~job:map
    ~merge:reduce
    ~neutral:init
    ~next:(Bucket.make ~num_workers:number_of_workers work)


let iter scheduler ~f work =
  map_reduce
    scheduler
    ~init:()
    ~map:(fun _ work -> f work)
    ~reduce:(fun _ _ -> ())
    work


let single_job { workers; _ } ~f work =
  let rec wait_until_ready handle =
    let { Worker.readys; _ } = Worker.select [handle] in
    match readys with
    | [] -> wait_until_ready handle
    | ready :: _ -> ready
  in
  match workers with
  | worker::_ ->
    Worker.call worker f work
    |> wait_until_ready
    |> Worker.get_result
  | [] ->
    failwith "This service contains no workers"


let mock () =
  Memory.get_heap_handle () |> ignore;
  { workers = []; number_of_workers = 1; bucket_multiplier = 1 }


let destroy _ =
  Worker.killall ()
