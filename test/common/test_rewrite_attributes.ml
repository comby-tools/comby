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
    |}];
