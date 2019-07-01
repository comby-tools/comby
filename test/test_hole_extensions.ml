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
  [%expect_exact {|
{foo.}
{foo.bar.quux}
{derp}
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

let%expect_test "alphanum_partial_match" =
  let run = run_all in
  let source = {|   foo.     foo.bar.quux derp|} in
  let match_template = {|foo.b:[x]r.quux|} in
  let rewrite_template = {|{:[x]}|} in
  run source match_template rewrite_template;
  [%expect_exact {|   foo.     {a} derp|}]
