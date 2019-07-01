open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run_all ?(configuration = configuration) source match_template rewrite_template =
  Generic.all ~configuration ~template:match_template ~source
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

(* can't activate because exit 1 jank being done *)
(*
let%expect_test "posix_parse_error" =
  let run = run_all in
  let source = {|foo|} in
  let match_template = {|:[x|[:derp:]]|} in
  let rewrite_template = {|:[x]|} in
  run source match_template rewrite_template;
  [%expect_exact {|No matches.|}]
*)

let%expect_test "posix_parse" =
  let run = run_all in
  let source = {|foo|} in
  let match_template = {|:[x|[:alnum:]]|} in
  let rewrite_template = {|:[x]|} in
  run source match_template rewrite_template;
  [%expect_exact {|No matches.|}]
