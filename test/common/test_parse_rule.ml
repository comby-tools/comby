open Core
open Comby_kernel
open Matchers

let rule_parses rule =
  match Rule.create rule with
  | Ok _ -> "true"
  | Error _ -> "false"

let%expect_test "parse_rule" =
  let rule = {| where :[1] == :[2], :[3] == "y" |} in
  rule_parses rule |> print_string;
  [%expect_exact {|true|}];
  let rule = {| where :[1] == :[2], :[3] != "x" |} in
  rule_parses rule |> print_string;
  [%expect_exact {|true|}];
  let rule = {| where :[1] != :[3] |} in
  rule_parses rule |> print_string;
  [%expect_exact {|true|}];
  let rule = {| where :[1] != :[3], |} in
  rule_parses rule |> print_string;
  [%expect_exact {|true|}]

let%expect_test "parse_basic" =
  Rule.create {|where "a" == "a"|}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact {|(rule ((Equal (String a) (String a))))
|}]

let%expect_test "parse_option_nested" =
  Rule.create {|where nested, "a" == "a" |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact {|(rule ((Option nested) (Equal (String a) (String a))))
|}]

let%expect_test "parse_match_one_case" =
  Rule.create {|where match "match_me" { | "case_one" -> true }|}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule ((Match (String match_me) (((String case_one) (True))))))\n"]

let%expect_test "parse_match_multi_case" =
  Rule.create
    {| where
       match "match_me" {
       | "case_one" -> true
       | "case_two" -> false
       }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match (String match_me)\n\
    \   (((String case_one) (True)) ((String case_two) (False))))))\n"]

let%expect_test "parse_case_optional_trailing" =
  Rule.create
    {| where
       match "match_me" {
       | "case_one" -> true,
       | "case_two" -> false
       }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match (String match_me)\n\
    \   (((String case_one) (True)) ((String case_two) (False))))))\n"]

let%expect_test "parse_case_optional_trailing" =
  Rule.create
    {| where
       match "match_me" {
       | "case_one" -> true,
       | "case_two" -> false
       }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match (String match_me)\n\
    \   (((String case_one) (True)) ((String case_two) (False))))))\n"]

let%expect_test "parse_freeform_antecedent_pattern" =
  Rule.create
    {| where
       match "match_me" {
       | case one -> true,
       | case two -> false
       | :[template] :[example] -> false
       }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match (String match_me)\n\
    \   (((String \"case one\") (True)) ((String \"case two\") (False))\n\
    \    ((Template\n\
    \      ((Hole\n\
    \        ((variable template) (pattern :[template]) (offset 0) (kind Value)))\n\
    \       (Constant \" \")\n\
    \       (Hole\n\
    \        ((variable example) (pattern :[example]) (offset 12) (kind Value)))))\n\
    \     (False))))))\n"]

let%expect_test "optional_first_pipe_one_case" =
  Rule.create {|
       where match "match_me" { thing -> true, }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule ((Match (String match_me) (((String thing) (True))))))\n"]

let%expect_test "optional_first_pipe_multiple_cases" =
  Rule.create {|
       where match "match_me" { thing -> true, | other -> true }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n ((Match (String match_me) (((String thing) (True)) ((String other) (True))))))\n"]

let%expect_test "parse_freeform_antecedent_pattern_single_quote" =
  Rule.create
    {|
       where match "match_me" {
         '"ni\'ce"' -> true
         | `multi
line
` -> true
       }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match (String match_me)\n\
    \   (((String \"\\\"ni\\\\'ce\\\"\") (True)) ((String  \"multi\\\n\
    \                                             \\nline\\\n\
    \                                             \\n\") (True))))))\n"]

let%expect_test "parse_freeform_antecedent_pattern_map_regex" =
  Rule.create
    {| where
       match "match_me" {
       | ~match_me -> true,
       | _ -> false,
       }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match (String match_me)\n\
    \   (((Template\n\
    \      ((Hole ((variable \"\") (pattern :[~match_me]) (offset 0) (kind Value)))))\n\
    \     (True))\n\
    \    ((Template\n\
    \      ((Hole ((variable _) (pattern :[_]) (offset 0) (kind Value)))))\n\
    \     (False))))))\n"]

let%expect_test "parse_regex_hole" =
  Rule.create
    {|
     where match :[1] {
     | ":[~^\\d+$]" -> false
     | ":[_]" -> true
     }
  |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match\n\
    \   (Template ((Hole ((variable 1) (pattern :[1]) (offset 0) (kind Value)))))\n\
    \   (((Template\n\
    \      ((Hole ((variable \"\") (pattern \":[~^\\\\d+$]\") (offset 0) (kind Value)))))\n\
    \     (False))\n\
    \    ((Template\n\
    \      ((Hole ((variable _) (pattern :[_]) (offset 0) (kind Value)))))\n\
    \     (True))))))\n"]

let%expect_test "parse_interpreting_escapes" =
  Rule.create
    {| where
       match "match_me" {
       | "\n\\" -> true,
       | `a\n\heh
b` -> false,
       }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match (String match_me)\n\
    \   (((String  \"\\\n\
    \             \\n\\\\\") (True)) ((String  \"a\\\\n\\\\heh\\\n\
    \                                     \\nb\") (False))))))\n"]

let%expect_test "parse_freeform_antecedent_in_rewrite_rule" =
  Rule.create {|
      where rewrite :[contents] { concat [:[x]] -> "nice" }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Rewrite\n\
    \   (Template\n\
    \    ((Hole\n\
    \      ((variable contents) (pattern :[contents]) (offset 0) (kind Value)))))\n\
    \   ((Template\n\
    \     ((Constant \"concat [\")\n\
    \      (Hole ((variable x) (pattern :[x]) (offset 8) (kind Value)))\n\
    \      (Constant ])))\n\
    \    (String nice)))))\n"]

let%expect_test "parse_freeform_consequent_in_rewrite_rule" =
  Rule.create {| where
       rewrite :[0] { :[1] :[2] -> :[1] a }
    |}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Rewrite\n\
    \   (Template ((Hole ((variable 0) (pattern :[0]) (offset 0) (kind Value)))))\n\
    \   ((Template\n\
    \     ((Hole ((variable 1) (pattern :[1]) (offset 0) (kind Value)))\n\
    \      (Constant \" \")\n\
    \      (Hole ((variable 2) (pattern :[2]) (offset 5) (kind Value)))))\n\
    \    (Template\n\
    \     ((Hole ((variable 1) (pattern :[1]) (offset 0) (kind Value)))\n\
    \      (Constant \" a\")))))))\n"]

let%expect_test "this_damn_rule" =
  Rule.create {|
  where match :[1] {
 | ":[~^\\d+$]" -> false
 | ":[_]" -> true
 }
|}
  |> Or_error.ok_exn
  |> fun rule ->
  print_s [%message (rule : Ast.expression list)];
  [%expect_exact
    "(rule\n\
    \ ((Match\n\
    \   (Template ((Hole ((variable 1) (pattern :[1]) (offset 0) (kind Value)))))\n\
    \   (((Template\n\
    \      ((Hole ((variable \"\") (pattern \":[~^\\\\d+$]\") (offset 0) (kind Value)))))\n\
    \     (False))\n\
    \    ((Template\n\
    \      ((Hole ((variable _) (pattern :[_]) (offset 0) (kind Value)))))\n\
    \     (True))))))\n"]
