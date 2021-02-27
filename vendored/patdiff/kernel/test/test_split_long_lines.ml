open! Core_kernel
open! Import
open Patdiff_kernel
module Patdiff_core = Patdiff_core.Without_unix

let%expect_test "refine does not raise with ~split_long_lines:true and no controlling tty"
  =
  let keep_ws = false in
  let hunks =
    Patdiff_core.diff
      ~context:Configuration.default_context
      ~line_big_enough:Configuration.default_line_big_enough
      ~keep_ws
      ~prev:[| "hello"; "world" |]
      ~next:[| "good bye"; "world" |]
  in
  let hunks =
    Patdiff_core.refine
      ~rules:Format.Rules.default
      ~output:Ascii
      ~split_long_lines:true
      ~produce_unified_lines:false
      ~keep_ws
      ~interleave:true
      ~word_big_enough:Configuration.default_word_big_enough
      hunks
  in
  print_s [%sexp (hunks : Hunks.t)];
  [%expect
    {|
    ((
      (prev_start 1)
      (prev_size  2)
      (next_start 1)
      (next_size  2)
      (ranges (
        (Replace
          (hello)
          ("good bye"))
        (Same ((world world))))))) |}]
;;
