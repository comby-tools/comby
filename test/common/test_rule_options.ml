open Core
open Test_helpers
open Comby_kernel
open Matchers

let%expect_test "strict_rule_no_holes" =
  let source = {|
foo( bar) foo(bar) foo( bar ) foo(bar )
foo(
   bar
)
|} in
  let match_template = {|foo(bar)|} in
  let rewrite_template = {|>yes<|} in
  let rule = "where true" in
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|
>yes< >yes< >yes< >yes<
>yes<
|}];
  let rule = "where strict" in
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|
foo( bar) >yes< foo( bar ) foo(bar )
foo(
   bar
)
|}]

let%expect_test "strict_rule_with_holes" =
  let source = {|foo(   bar   )|} in
  let match_template = {|foo(:[1])|} in
  let rewrite_template = {|:[1]|} in
  let rule = "where true" in
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|   bar   |}];
  let source = {|foo(   bar,x  )|} in
  let match_template = {|foo(bar,:[1])|} in
  let rewrite_template = {|:[1]|} in
  let rule = "where true" in
  run (module Omega.Generic) source match_template rewrite_template ~rule;
  [%expect_exact {|x  |}]
