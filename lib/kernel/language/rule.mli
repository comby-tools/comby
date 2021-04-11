open Core_kernel

include module type of Types

val create : string -> t Or_error.t

module Alpha : Engine
module Omega : Engine
