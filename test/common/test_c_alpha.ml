open Core

open Rewriter

open Test_helpers

open Test_alpha

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

let%expect_test "comments_1" =
  let source = {|match this /**/ expect end|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|:[1]|} in

  run source match_template rewrite_template;
  [%expect_exact {|expect|}]

let%expect_test "comments_2" =
  let source = {|match this /* */ expect end|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|:[1]|} in

  run source match_template rewrite_template;
  [%expect_exact {|expect|}]

let%expect_test "comments_3" =
  let source = {|match this /* blah blah */ expect /**/ end|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|:[1]|} in

  run source match_template rewrite_template;
  [%expect_exact {|expect|}]

let%expect_test "comments_4" =
  let source = {|match this expect/**/end|} in
  let match_template = {|match this :[1]end|} in
  let rewrite_template = {|:[1]|} in

  run source match_template rewrite_template;
  [%expect_exact {|expect|}]

let%expect_test "comments_5" =
  let source = {|match this expect /**/end|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|:[1]|} in

  run source match_template rewrite_template;
  [%expect_exact {|expect|}]

let%expect_test "comments_6" =
  let source = {|/* don't match this (a) end */|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|nothing matches|} in

  run source match_template rewrite_template;
  [%expect_exact {|nothing matches|}]

let%expect_test "comments_7" =
  let source = {|/* don't match /**/ this (a) end */|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|nothing matches|} in

  run source match_template rewrite_template;
  [%expect_exact {|nothing matches|}]

let%expect_test "comments_8" =
  let source = {|(/* don't match this (a) end */)|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|nothing matches|} in

  run source match_template rewrite_template;
  [%expect_exact {|nothing matches|}]

let%expect_test "comments_9" =
  let source = {|/* don't match this (a) end */ do match this (b) end|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|:[1]|} in

  run source match_template rewrite_template;
  [%expect_exact {|/* don't match this (a) end */ do (b)|}]

let%expect_test "comments_10" =
  let source = {|/* don't match this (a) end */ do match this () end|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|:[1]|} in

  run source match_template rewrite_template;
  [%expect_exact {|/* don't match this (a) end */ do ()|}]

let%expect_test "comments_11" =
  let source = {|do match this (b) end /* don't match this (a) end */|} in
  let match_template = {|match this :[1] end|} in
  let rewrite_template = {|:[1]|} in

  run source match_template rewrite_template;
  [%expect_exact {|do (b) /* don't match this (a) end */|}]
