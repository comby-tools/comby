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

let%expect_test "lines" =
  let source = {|
    {
      foo
    }
    {
      bar
      baz
    }
|} in
  let match_template = "{:[x]}" in
  let rewrite_template = {|:[x].lines|}
  in
  run (module Matchers.Alpha.Generic) source match_template rewrite_template;
  [%expect_exact {|
    2
    3
|}];
  run (module Matchers.Omega.Generic) source match_template rewrite_template;
  [%expect_exact {|
    2
    3
|}]

let%expect_test "offsets" =
  let source = {|
first
<  >things
<     >first
|} in
  let match_template = ":[[x]]" in
  let rewrite_template = {|
offset: :[[x]].offset
offset.start: :[[x]].offset.start
offset.end: :[[x]].offset.end
line.start: :[[x]].line.start     // can't compute without source yet
line.end: :[[x]].line.end         // can't compute without source yet
column.start: :[[x]].column.start // can't compute without source yet
column.end: :[[x]].column.end     // can't compute without source yet
|}
  in
  run (module Matchers.Alpha.Generic) source match_template rewrite_template;
  [%expect_exact {|

offset: 1
offset.start: 1
offset.end: 6
line.start: :[[x]].line.start     // can't compute without source yet
line.end: :[[x]].line.end         // can't compute without source yet
column.start: :[[x]].column.start // can't compute without source yet
column.end: :[[x]].column.end     // can't compute without source yet

<  >
offset: 11
offset.start: 11
offset.end: 17
line.start: :[[x]].line.start     // can't compute without source yet
line.end: :[[x]].line.end         // can't compute without source yet
column.start: :[[x]].column.start // can't compute without source yet
column.end: :[[x]].column.end     // can't compute without source yet

<     >
offset: 25
offset.start: 25
offset.end: 30
line.start: :[[x]].line.start     // can't compute without source yet
line.end: :[[x]].line.end         // can't compute without source yet
column.start: :[[x]].column.start // can't compute without source yet
column.end: :[[x]].column.end     // can't compute without source yet

|}];
  run (module Matchers.Omega.Generic) source match_template rewrite_template;
  [%expect_exact {|

offset: 1
offset.start: 1
offset.end: 6
line.start: :[[x]].line.start     // can't compute without source yet
line.end: :[[x]].line.end         // can't compute without source yet
column.start: :[[x]].column.start // can't compute without source yet
column.end: :[[x]].column.end     // can't compute without source yet

<  >
offset: 11
offset.start: 11
offset.end: 17
line.start: :[[x]].line.start     // can't compute without source yet
line.end: :[[x]].line.end         // can't compute without source yet
column.start: :[[x]].column.start // can't compute without source yet
column.end: :[[x]].column.end     // can't compute without source yet

<     >
offset: 25
offset.start: 25
offset.end: 30
line.start: :[[x]].line.start     // can't compute without source yet
line.end: :[[x]].line.end         // can't compute without source yet
column.start: :[[x]].column.start // can't compute without source yet
column.end: :[[x]].column.end     // can't compute without source yet

|}]
