(** Copyright (c) 2016-present, Facebook, Inc.
    Modified work Copyright (c) 2018-2019 Rijnard van Tonder
    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Hack_parallel_intf.Std

module Daemon = Daemon

type t

val create: ?number_of_workers: int -> ?bucket_multiplier: int -> unit -> t

val map_reduce
  :  t
  -> ?bucket_size: int
  -> init:'a
  -> map:('a -> 'b list -> 'c)
  -> reduce:('c -> 'a -> 'a)
  -> 'b list
  -> 'a

val iter: t -> f: ('a list -> unit) -> 'a list -> unit

val single_job : t -> f:('a -> 'b) -> 'a -> 'b

val mock: unit -> t

val destroy : t -> unit
