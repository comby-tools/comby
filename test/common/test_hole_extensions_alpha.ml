open Core

open Rewriter

open Test_helpers

include Test_alpha

let run_all ?(configuration = configuration) source match_template rewrite_template =
  Generic.all ~configuration ~template:match_template ~source ()
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

let%expect_test "non_space" =
  let run = run_all in
  let source = {|   foo.     foo.bar.quux derp|} in
  let match_template = {|:[x.]|} in
  let rewrite_template = {|{:[x]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|   {foo.}     {foo.bar.quux} {derp}|}]

let%expect_test "only_space" =
  let run = run_all in
  let source = {|   foo.     foo.bar.quux derp|} in
  let match_template = {|:[ x]|} in
  let rewrite_template = {|{:[x]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|{   }foo.{     }foo.bar.quux{ }derp|}]

let%expect_test "up_to_newline" =
  let run = run_all in
  let source =
    {|
foo.
foo.bar.quux
derp
|} in
  let match_template = {|:[x\n]|} in
  let rewrite_template = {|{:[x]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|{
}{foo.
}{foo.bar.quux
}{derp
}|}]

let%expect_test "match_empty_in_newline_hole" =
  let run = run_all in
  let source =
    {|stuff
after
|} in
  let match_template = {|stuff:[x\n]|} in
  let rewrite_template = {|{->:[x]<-}|} in
  run source match_template rewrite_template;
  [%expect_exact {|{->
<-}after
|}]

let%expect_test "leading_indentation" =
  let run = run_all in
  let source =
    {|
       foo. bar bazz
          foo.bar.quux
  derp
|} in
  let match_template = {|:[ leading_indentation]:[rest\n]|} in
  let rewrite_template = {|{:[leading_indentation]}:[rest]|} in
  run source match_template rewrite_template;
  [%expect_exact {|
{       }foo. bar bazz
{          }foo.bar.quux
{  }derp
|}]

let%expect_test "non_space_partial_match" =
  let run = run_all in
  let source = {|   foo.     foo.bar.quux derp|} in
  let match_template = {|foo.:[x.]ux|} in
  let rewrite_template = {|{:[x]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|   foo.     {bar.qu} derp|}]

let%expect_test "non_space_does_not_match_reserved_delimiters" =
  let run = run_all in
  let source = {|fo.o(x)|} in
  let match_template = {|:[f.]|} in
  let rewrite_template = {|{:[f]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|{fo.o}({x})|}]

let%expect_test "non_space_only_hole" =
  let run = run_all in
  let source = {|a.b c.d|} in
  let match_template = {|:[x.]|} in
  let rewrite_template = {|>:[x]<|} in
  run source match_template rewrite_template;
  [%expect_exact {|>a.b< >c.d<|}]

let%expect_test "alphanum_partial_match" =
  let run = run_all in
  let source = {|   foo.     foo.bar.quux derp|} in
  let match_template = {|foo.b:[x]r.quux|} in
  let rewrite_template = {|{:[x]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|   foo.     {a} derp|}]

let%expect_test "newline_matcher_should_not_be_sat_on_space" =
  let run = run_all in
  let source =
    {|a b c d
 e f g h|} in
  let match_template = {|:[line\n] |} in
  let rewrite_template = {|{:[line]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|{a b c d
}e f g h|}];

  let run = run_all in
  let source =
    {|a b c d
 e f g h|} in
  let match_template = {|:[line\n]:[next]|} in
  let rewrite_template = {|{:[line]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|{a b c d
}|}];

  let run = run_all in
  let source =
    {|a b c d
e f g h
|} in
  let match_template = {|:[line1\n]:[next\n]|} in
  let rewrite_template = {|{:[line1]|:[next]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|{a b c d
|e f g h
}|}]

let%expect_test "implicit_equals" =
  let run = run_all in
  let source = {|a b a|} in
  let match_template = {|:[[x]] :[[m]] :[[x]]|} in
  let rewrite_template = {|:[m]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b|}]

let%expect_test "implicit_equals_does_not_apply_to_underscore" =
  let run = run_all in
  let source = {|a b c|} in
  let match_template = {|:[[x]] :[[_]] :[[_]]|} in
  let rewrite_template = {|:[x]|} in
  run source match_template rewrite_template;
  [%expect_exact {|a|}]

let%expect_test "expression_hole" =
  let run = run_all in
  let source = {|(b, c, d) [ ] { { } } { } ()()|} in
  let match_template = {|:[x:e]|} in
  let rewrite_template = {|>:[x]<|} in
  run source match_template rewrite_template;
  [%expect_exact {|>(b, c, d)< >[ ]< >{ { } }< >{ }< >()()<|}]

let%expect_test "expression_hole" =
  let run = run_all in
  let source = {|a(b, c, d)e [][] { { } }|} in
  let match_template = {|:[x:e]|} in
  let rewrite_template = {|>:[x]<|} in
  run source match_template rewrite_template;
  [%expect_exact {|>a(b, c, d)e< >[][]< >{ { } }<|}]
