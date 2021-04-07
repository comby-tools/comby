open! Core_kernel
open! Import

type t =
  { name : string
  ; text : string
  }
[@@deriving fields]
