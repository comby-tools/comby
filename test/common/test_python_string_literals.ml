open Core
open Comby_kernel
open Matchers
open Test_helpers

let all ~template ~source = Python.all ~configuration ~template ~source ()

let%expect_test "matched_contains_raw_literal_quotes" =
  let source = {|"""blah"""|} in
  let template = {|""":[[1]]"""|} in
  all ~template ~source |> print_only_match;
  [%expect_exact {|[ "\"\"\"blah\"\"\"" ]|}]

let%expect_test "interpreted_string_does_not_match_raw_literal" =
  let source = {|"""blah""" "blah"|} in
  let template = {|":[[1]]"|} in
  all ~template ~source |> print_only_match;
  [%expect_exact {|[ "\"blah\"" ]|}]

let%expect_test "interpreted_string_does_not_match_raw_literal_containing_quote" =
  let source = {|"""blah""" """bl"ah""" "blah"|} in
  let template = {|":[[1]]"|} in
  all ~template ~source |> print_only_match;
  [%expect_exact {|[ "\"blah\"" ]|}]

let%expect_test "raw_string_matches_string_containing_quote" =
  let source = {|"""bl"ah"""|} in
  let template = {|""":[1]"""|} in
  all ~template ~source |> print_only_match;
  [%expect_exact {|[ "\"\"\"bl\"ah\"\"\"" ]|}]

let%expect_test "invalid_raw_string_in_python_but_matches_because_ignores_after" =
  let source = {|"""""""|} in
  let template = {|""":[1]"""|} in
  all ~template ~source |> print_only_match;
  [%expect_exact {|[ "\"\"\"\"\"\"" ]|}]

let%expect_test "raw_string_captures_escape_sequences" =
  let source = {|"""\""""|} in
  let template = {|""":[1]"""|} in
  all ~template ~source |> print_only_match;
  [%expect_exact {|[ "\"\"\"\\\"\"\"" ]|}]
