open Core

open Configuration

let%expect_test "basic" =
  let match_template = "for :[i], :[x] := range :[list] {:[body]}" in
  let spec = Specification.create ~match_template () in
  let result = Specification.to_regex spec in
  print_string result;
  [%expect_exact {|(for\s+)(?s:.)*?(,\s+)(?s:.)*?(\s+:=\s+range\s+)(?s:.)*?(\s+\{)(?s:.)*?(\})|}];
