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
      where rewrite :[1] { "int" -> "expect" }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|expect|}]

let%expect_test "sequenced_rewrite_rule" =
  let source = {|{ { a : { b : { c : d } } } }|} in
  let match_template = {|{ :[a] : :[rest] }|} in
  let rewrite_template = {|{ :[a] : :[rest] }|} in

  let rule =
    {|
      where
      rewrite :[a] { "a" -> "qqq" },
      rewrite :[rest] { "{ b : { :[other] } }" -> "{ :[other] }" }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|{ { qqq : { c : d } } }|}]

let%expect_test "rewrite_rule_for_list" =
  let source = {|[1, 2, 3, 4,]|} in
  let match_template = {|[:[contents]]|} in
  let rewrite_template = {|[:[contents]]|} in

  let rule =
    {|
      where rewrite :[contents] { ":[[x]]," -> ":[[x]];" }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|[1; 2; 3; 4;]|}]

let%expect_test "rewrite_rule_for_list_strip_last" =
  let source = {|[1, 2, 3, 4]|} in
  let match_template = {|[:[contents]]|} in
  let rewrite_template = {|[:[contents]]|} in

  let rule =
    {|
      where rewrite :[contents] { ":[x], " -> ":[x]; " }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|[1; 2; 3; 4]|}]

let%expect_test "haskell_example" =
  let source = {|
     (concat
     [ "blah blah blah"
     , "blah"
     ])
|} in
  let match_template = {|(concat [:[contents]])|} in
  let rewrite_template = {|(:[contents])|} in

  let rule =
    {|
      where rewrite :[contents] { "," -> "++" }
    |}
    |> Rule.create
    |> Or_error.ok_exn
  in

  run_rule source match_template rewrite_template rule;
  [%expect_exact {|
     ( "blah blah blah"
     ++ "blah"
     )
|}]
