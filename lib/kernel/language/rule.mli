open Core_kernel

include module type of Types

val create : string -> t Or_error.t

include Engine
