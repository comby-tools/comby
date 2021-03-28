open! Core_kernel
open! Import

type t =
  | Binary_same
  | Binary_different of
      { prev_is_binary : bool
      ; next_is_binary : bool
      }
  | Hunks of Hunks.t

let update_config_infer_keep_ws config ~prev ~next =
  let keep_ws =
    config.Configuration.keep_ws || Should_keep_whitespace.for_diff ~prev ~next
  in
  Configuration.override ~keep_ws config
;;

let create
      (config : Configuration.t)
      ~(prev : Diff_input.t)
      ~(next : Diff_input.t)
      ~compare_assuming_text
  =
  let prev_is_binary, next_is_binary =
    match config.assume_text with
    | true -> false, false
    | false -> Is_binary.string prev.text, Is_binary.string next.text
  in
  if prev_is_binary || next_is_binary
  then
    if String.( = ) prev.text next.text
    then Binary_same
    else Binary_different { prev_is_binary; next_is_binary }
  else
    Hunks
      (compare_assuming_text (update_config_infer_keep_ws config ~prev ~next) ~prev ~next)
;;

let has_no_diff t =
  match t with
  | Binary_same -> true
  | Binary_different _ -> false
  | Hunks hunks -> List.for_all hunks ~f:Patience_diff.Hunk.all_same
;;
