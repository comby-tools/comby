open Core
open Matchers

let configuration = Configuration.create ~match_kind:Fuzzy ()

let format s =
  let s = String.chop_prefix_exn ~prefix:"\n" s in
  let leading_indentation = Option.value_exn (String.lfindi s ~f:(fun _ c -> not (Char.equal c ' '))) in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"
  |> String.chop_suffix_exn ~suffix:"\n"

let print_matches matches =
  List.map matches ~f:Match.to_yojson
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

let print_only_match matches =
  List.map matches ~f:(fun matched -> `String matched.Match.matched)
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string
