open Core

open Comby_kernel
open Matchers

open Test_helpers

(* See https://stackoverflow.com/questions/6698039/nested-comments-in-c-c *)
let%expect_test "nested_multiline_ocaml" =
  let source = {|int nest = /*/*/ 0 */**/ 1;|} in
  let template = {|0 * 1|} in

  (* 0 is not commented out *)
  Alpha.C.all ~configuration ~template ~source ()
  |> print_only_match;
  [%expect_exact {|[ "0 */**/ 1" ]|}];
  Omega.C.all ~configuration ~template ~source ()
  |> print_only_match;
  [%expect_exact {|[ "0 */**/ 1" ]|}];

  (* 0 is commented out *)
  Alpha.C_nested_comments.all ~configuration ~template ~source ()
  |> print_only_match;
  [%expect_exact {|[]|}];
  Omega.C_nested_comments.all ~configuration ~template ~source ()
  |> print_only_match;
  [%expect_exact {|[]|}];
