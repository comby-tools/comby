open Core

open Language
open Matchers
open Match
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let format s =
  let s = String.chop_prefix_exn ~prefix:"\n" s in
  let leading_indentation = Option.value_exn (String.lfindi s ~f:(fun _ c -> c <> ' ')) in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"
  |> String.chop_suffix_exn ~suffix:"\n"

let run_rule source match_template rewrite_template rule =
  Generic.first ~configuration match_template source
  |> function
  | Error _ -> print_string "bad"
  | Ok result ->
    match result with
    | ({ environment; _ } as m) ->
      let e = Rule.(result_env @@ apply rule environment) in
      match e with
      | None -> print_string "bad bad"
      | Some e ->
        { m with environment = e }
        |> List.return
        |> Rewrite.all ~source ~rewrite_template
        |> (fun x -> Option.value_exn x)
        |> (fun { rewritten_source; _ } -> rewritten_source)
        |> print_string

let%expect_test "rewrite_rule" =
  let source = {|int|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  let rule =
    {|
      where rewrite :[1] {
      | "int" -> "expect"
      }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|expect|}]

let%expect_test "rewrite_rule" =
  let source = {|string expect|} in
  let match_template = {|:[1] :[2]|} in
  let rewrite_template = {|:[2]|} in

  let rule =
    {|
      where rewrite :[1] {
      | "int" -> "5"
      | "string" -> ":[2]"
      }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|expect|}]

let%expect_test "conditional_rewrite_rule" =
  let source = {|{ { a : { b : { c : d } } } }|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  let rule =
    {|
      where rewrite :[1] {
      | "{ :[a] : :[rest] }" -> "a" == :[a], "doot"
      }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|doot|}]

let%expect_test "rewrite_rule_using_match_result" =
  let source = {|{ { a : { b : { c : d } } } }|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  let rule =
    {|
      where rewrite :[1] {
      | "{ :[a] : :[rest] }" -> "a" == :[a], ":[rest]"
      }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|{ b : { c : d } }|}];

  let source = {|{ { a : { b : { c : d } } } }|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in
  let rule =
    {|
      where rewrite :[1] {
      | "{ :[a] : :[rest] }" -> "b" == :[a], ":[rest]"
      }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|{ { a : { b : { c : d } } } }|}]


let%expect_test "nested_rewrite_rule" =
  let source = {|{ { a : { b : { c : d } } } }|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in

  let rule =
    {|
      where
      rewrite :[1] {
      | "{ :[a] : :[rest] }" ->
            rewrite :[a] {
            | "a" -> "b"
            }, "{ :[a] : :[rest] }"
      }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|{ b : { b : { c : d } } }|}]

let%expect_test "sequenced_rewrite_rule" =
  let source = {|{ { a : { b : { c : d } } } }|} in
  let match_template = {|{ :[a] : :[rest] }|} in
  let rewrite_template = {|{ :[a] : :[rest] }|} in

  let rule =
    {|
      where
      rewrite :[a] {
      | "a" -> "qqq"
      },
      rewrite :[rest] {
      | "{ b : { :[other] } }" -> "{ :[other] }"
      }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|{ { qqq : { c : d } } }|}]
