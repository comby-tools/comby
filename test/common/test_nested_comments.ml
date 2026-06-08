open Core
open Comby_kernel
open Matchers
open Test_helpers

(* See https://stackoverflow.com/questions/6698039/nested-comments-in-c-c *)
let%expect_test "nested_multiline_c" =
  let source = {|int nest = /*/*/ 0 */**/ 1;|} in
  let template = {|0 * 1|} in
  (* 0 is not commented out *)
  Alpha.C.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[ "0 */**/ 1" ]|}];
  Omega.C.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[ "0 */**/ 1" ]|}];
  (* 0 is commented out *)
  Alpha.C_nested_comments.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}];
  Omega.C_nested_comments.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]

let%expect_test "nested_multiline_ocaml_comments_are_skipped" =
  let source = {|(* let commented = 2 (* nested *) *)
let live = 1|} in
  let template = {|let commented = :[value]|} in
  Alpha.OCaml.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}];
  Omega.OCaml.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]

let%expect_test "nested_multiline_haskell_comments_are_skipped" =
  let source = {|{- let commented = 2 {- nested -} -}
let live = 1|} in
  let template = {|let commented = :[value]|} in
  Alpha.Haskell.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}];
  Omega.Haskell.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]

let%expect_test "nested_multiline_nim_comments_are_skipped" =
  let source = {|#[ let commented = 2 #[ nested ]# ]#
let live = 1|} in
  let template = {|let commented = :[value]|} in
  Alpha.Nim.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}];
  Omega.Nim.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]

let%expect_test "nested_multiline_julia_comments_are_skipped" =
  let source = {|#= commented = 2 #= nested =# =#
live = 1|} in
  let template = {|commented = :[value]|} in
  Alpha.Julia.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}];
  Omega.Julia.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]
