open! Core_kernel
open! Import

val apply_styles : ?drop_leading_resets:bool -> Format.Style.t list -> string -> string

include Output.S
