open Core

open Test_helpers

include Test_omega

(* See https://stackoverflow.com/questions/6698039/nested-comments-in-c-c *)
let%expect_test "nested_multiline_ocaml" =
  let source = {|int nest = /*/*/ 0 */**/ 1;|} in
  let template = {|0 * 1|} in
  (* 0 is not commented out *)
  let matches_no_nesting = C.all ~configuration ~template ~source in
  print_only_match matches_no_nesting;
  [%expect_exact {|[ "0 */**/ 1" ]|}];

  let matches_no_nesting = C_nested_comments.all ~configuration ~template ~source in
  (* 0 is commented out *)
  print_only_match matches_no_nesting;
  [%expect_exact {|[]|}];
