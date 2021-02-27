open! Core_kernel
open! Import
include Output_intf

type t =
  | Ansi
  | Ascii
  | Html
[@@deriving compare, sexp]

let implies_unrefined t =
  match t with
  | Ansi | Html -> false
  | Ascii -> true
;;
