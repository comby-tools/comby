open Core

open Test_helpers
open Comby_kernel
open Matchers

let%expect_test "whitespace_should_not_matter_between_separators" =
  let source = {|*p|} in
  let match_template = {|*:[1]|} in
  let rewrite_template = {|:[1]|} in
  run (module Alpha.C) source match_template rewrite_template;
  [%expect_exact {|p|}];
  run (module Omega.C) source match_template rewrite_template;
  [%expect_exact {|p|}];

  let source = {|*          p|} in
  let match_template = {|*:[1]|} in
  let rewrite_template = {|:[1]|} in
  run (module Alpha.C) source match_template rewrite_template;
  [%expect_exact {|          p|}];
  run (module Omega.C) source match_template rewrite_template;
  [%expect_exact {|          p|}];

  let source = {|*          p|} in
  let match_template = {|* :[1]|} in
  let rewrite_template = {|:[1]|} in
  run (module Alpha.C) source match_template rewrite_template;
  [%expect_exact {|p|}];
  run (module Omega.C) source match_template rewrite_template;
  [%expect_exact {|p|}]
