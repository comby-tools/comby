open Matchers
module Script = Script.Make (Metasyntax.Default) (External.Default)

let run input =
  let result =
    match Script.parse input with
    | Ok result -> Script.to_string result
    | Error _ -> "ERROR"
  in
  print_string result

let%expect_test "test_script_basic_sequence" =
  let script = {|:[x] -> :[y] where nested---|} in
  run script;
  [%expect_exact {|ERROR|}];
  let script = {|:[x] -> :[y] where nested  ---  |} in
  run script;
  [%expect_exact {|ERROR|}];
  let script = {|:[x] -> :[y] where nested|} in
  run script;
  [%expect_exact {|ERROR|}];
  let script = {|
      :[x] -> :[y] where nested  ---
      :[x] -> :[y] where nested
  |} in
  run script;
  [%expect_exact {|ERROR|}]

let%expect_test "test_script_optional_rewrite" =
  let script = {|:[x] where nested|} in
  run script;
  [%expect_exact {|ERROR|}];
  let script = {|:[x] where nested--- :[y] where nested|} in
  run script;
  [%expect_exact {|ERROR|}]

let%expect_test "test_script_optional_rule" =
  let script = {|:[x]|} in
  run script;
  [%expect_exact {|ERROR|}];
  let script = {|:[x]--- :[y]--- :[z] -> :[q]|} in
  run script;
  [%expect_exact {|ERROR|}]

let%expect_test "test_spec_expressions" =
  let script = {|:[x] -> :[y] where nested or :[y] -> :[t] where nested---|} in
  run script;
  [%expect_exact {|ERROR|}];
  let script =
    {|
      ---
      :[x] where nested or :[y] -> :[t] where nested
      ---
      not :[x] and :[y] or :[z]
  |}
  in
  run script;
  [%expect_exact {|ERROR|}]
