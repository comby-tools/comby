open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run source match_template rewrite_template =
  C.first ~configuration match_template source
  |> function
  | Ok result ->
    Rewrite.all ~source ~rewrite_template [result]
    |> (fun x -> Option.value_exn x)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string
  | Error _ ->
    print_string rewrite_template

let%expect_test "whitespace_should_not_matter_between_separators" =
  let source = {|*p|} in
  let match_template = {|*:[1]|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|p|}];

  let source = {|*          p|} in
  let match_template = {|*:[1]|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|          p|}];

  let source = {|*          p|} in
  let match_template = {|* :[1]|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|p|}]
