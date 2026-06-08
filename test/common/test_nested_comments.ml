open Core
open Comby_kernel
open Matchers
open Test_helpers

(* See https://stackoverflow.com/questions/6698039/nested-comments-in-c-c *)
let%expect_test "nested_multiline_c" =
  let source = {|int nest = /*/*/ 0 */**/ 1;|} in
  let template = {|0 * 1|} in
  C.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[ "0 */**/ 1" ]|}];
  C_nested_comments.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]

let%expect_test "nested_multiline_ocaml_comments_are_skipped" =
  let source = {|(* let commented = 2 (* nested *) *)
let live = 1|} in
  let template = {|let commented = :[value]|} in
  OCaml.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]

let%expect_test "nested_multiline_haskell_comments_are_skipped" =
  let source = {|{- let commented = 2 {- nested -} -}
let live = 1|} in
  let template = {|let commented = :[value]|} in
  Haskell.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]

let%expect_test "nested_multiline_nim_comments_are_skipped" =
  let source = {|#[ let commented = 2 #[ nested ]# ]#
let live = 1|} in
  let template = {|let commented = :[value]|} in
  Nim.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]

let%expect_test "nested_multiline_julia_comments_are_skipped" =
  let source = {|#= commented = 2 #= nested =# =#
live = 1|} in
  let template = {|commented = :[value]|} in
  Julia.all ~configuration ~template ~source () |> print_only_match;
  [%expect_exact {|[]|}]
