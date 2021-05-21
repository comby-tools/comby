open Core

open Comby_kernel
open Test_helpers

let%expect_test "strings" =
  let source = {|
    LOWERCASE
    uppercase
    capitalize
    Uncapitalize
    upper_camel_case
    Lower_camel_Case
    upperSnakeCase
    lowerSnakeCase
|}
  in
  run (module Matchers.Alpha.Generic) source
    {|
       :[[a]]
       :[[b]]
       :[[c]]
       :[[d]]
       :[[e]]
       :[[f]]
       :[[g]]
       :[[h]]
    |}
    {|
       :[[a]].lowercase
       :[[b]].UPPERCASE
       :[[c]].Capitalize
       :[[d]].uncapitalize
       :[[e]].UpperCamelCase
       :[[f]].lowerCamelCase
       :[[g]].UPPER_SNAKE_CASE
       :[[h]].lower_snake_case
    |};
  [%expect_exact {|
       lowercase
       UPPERCASE
       Capitalize
       uncapitalize
       UpperCamelCase
       lowerCamelCase
       UPPER_SNAKE_CASE
       lower_snake_case
    |}]

let%expect_test "filepath_rewrite_template" =
  let source = {|whatever|} in
  let filepath = "this/is/a/path" in
  run (module Matchers.Alpha.Generic) ~filepath source ":[all]" "\n:[all].file.path\n:[all].file.name\n:[all].file.directory";
  [%expect_exact {|
this/is/a/path
path
this/is/a|}];
  run (module Matchers.Omega.Generic) ~filepath source ":[all]" "\n:[all].file.path\n:[all].file.name\n:[all].file.directory";
  [%expect_exact {|
this/is/a/path
path
this/is/a|}]

let%expect_test "filepath_rule" =
  let source = {|thing|} in
  let filepath = "this/is/a/path" in
  let match_template = ":[x]" in
  let rule = {|where rewrite :[x] { _ -> :[x].file.path }|} in
  run (module Matchers.Alpha.Generic) ~filepath source ~rule match_template "ok: :[x]";
  [%expect_exact {|ok: this/is/a/path|}];
  run (module Matchers.Omega.Generic) ~filepath source ~rule match_template "ok: :[x]";
  [%expect_exact {|ok: this/is/a/path|}]
