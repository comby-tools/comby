open Core
open Comby

let%expect_test "basic" =
  let match_template = "for :[i], :[x] := range :[list] {:[body]}" in
  let spec = Matchers.Specification.create ~match_template () in
  let result = Matchers.Specification.to_regex spec in
  print_string result;
  [%expect_exact {|(for\s+(\n|.)*?,\s+(\n|.)*?\s+:=\s+range\s+(\n|.)*?\s+\{(\n|.)*?\})|}]

let%expect_test "different_holes" =
  let match_template = "for :[[i]], :[ ] := range :[list.] {:[body]}" in
  let spec = Matchers.Specification.create ~match_template () in
  let result = Matchers.Specification.to_regex spec in
  print_string result;
  [%expect_exact
    {|(for\s+(\w+),\s+(\ |\t|\s|\r|\n)+\s+:=\s+range\s+([^ \t\s\r\n])+\s+\{(\n|.)*?\})|}]
