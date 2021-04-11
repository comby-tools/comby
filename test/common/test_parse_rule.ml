open Core

open Language

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

  let rule =  {| where :[1] != :[3] |} in
  rule_parses rule |> print_string;
  [%expect_exact {|true|}];

  let rule =  {| where :[1] != :[3], |} in
  rule_parses rule |> print_string;
  [%expect_exact {|true|}]

let%expect_test "parse_basic" =
  Rule.create {|where "a" == "a"|}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact {|(rule ((Equal (String a) (String a))))
|}]

let%expect_test "parse_option_nested" =
  Rule.create {|where nested, "a" == "a" |}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact {|(rule ((Option nested) (Equal (String a) (String a))))
|}]

let%expect_test "parse_match_one_case" =
  Rule.create {|where match "match_me" { | "case_one" -> true }|}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule ((Match (String match_me) (((String case_one) (True))))))
"]

let%expect_test "parse_match_multi_case" =
  Rule.create
    {| where
       match "match_me" {
       | "case_one" -> true
       | "case_two" -> false
       }
    |}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (String match_me)
   (((String case_one) (True)) ((String case_two) (False))))))
"]

let%expect_test "parse_case_optional_trailing" =
  Rule.create
    {| where
       match "match_me" {
       | "case_one" -> true,
       | "case_two" -> false
       }
    |}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (String match_me)
   (((String case_one) (True)) ((String case_two) (False))))))
"]

let%expect_test "parse_case_optional_trailing" =
  Rule.create
    {| where
       match "match_me" {
       | "case_one" -> true,
       | "case_two" -> false
       }
    |}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (String match_me)
   (((String case_one) (True)) ((String case_two) (False))))))
"]

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
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (String match_me)
   (((String \"case one\") (True)) ((String \"case two\") (False))
    ((String \":[template] :[example]\") (False))))))
"]

let%expect_test "optional_first_pipe_one_case" =
  Rule.create
    {|
       where match "match_me" { thing -> true, }
    |}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule ((Match (String match_me) (((String thing) (True))))))
"]

let%expect_test "optional_first_pipe_multiple_cases" =
  Rule.create
    {|
       where match "match_me" { thing -> true, | other -> true }
    |}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (String match_me) (((String thing) (True)) ((String other) (True))))))
"]

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
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (String match_me)
   (((String \"\\\"ni\\\\'ce\\\"\") (True)) ((String  \"multi\\
                                             \\nline\\
                                             \\n\") (True))))))
"]

let%expect_test "parse_freeform_antecedent_pattern_map_regex" =
  Rule.create
    {| where
       match "match_me" {
       | ~match_me -> true,
       | _ -> false,
       }
    |}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (String match_me)
   (((Variable :[~match_me]) (True)) ((Variable :[_]) (False))))))
"]

let%expect_test "parse_regex_hole" =
  Rule.create
    {|
     where match :[1] {
     | ":[~^\\d+$]" -> false
     | ":[_]" -> true
     }
  |}
  |> Or_error.ok_exn
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (Variable 1)
   (((String \":[~^\\\\d+$]\") (False)) ((String :[_]) (True))))))
"]

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
  |> fun rule -> print_s [%message (rule : Ast.expression list)];
  [%expect_exact "(rule
 ((Match (String match_me)
   (((String  \"\\
             \\n\\\\\") (True)) ((String  \"a\\\\n\\\\heh\\
                                     \\nb\") (False))))))
"]
