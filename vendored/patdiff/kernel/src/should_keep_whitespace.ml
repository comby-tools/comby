open! Core_kernel
open! Import

let looks_like_python_filename = String.is_suffix ~suffix:".py"

let looks_like_python_first_line first_line =
  String.is_prefix first_line ~prefix:"#!"
  && String.is_substring first_line ~substring:"python"
;;

let looks_like_python input ~get_name ~get_first_line =
  looks_like_python_filename (get_name input)
  || looks_like_python_first_line (get_first_line input)
;;

let for_diff_internal ~prev ~next ~get_name ~get_first_line =
  looks_like_python prev ~get_name ~get_first_line
  || looks_like_python next ~get_name ~get_first_line
;;

let for_diff =
  for_diff_internal
    ~get_name:Diff_input.name
    ~get_first_line:(fun (input : Diff_input.t) ->
      match String.lsplit2 input.text ~on:'\n' with
      | Some (first_line, _) -> first_line
      | None -> input.text)
;;

let for_diff_array =
  for_diff_internal ~get_name:fst ~get_first_line:(fun (_, lines) ->
    if Array.is_empty lines then "" else lines.(0))
;;
