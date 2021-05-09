open Core
open Comby_kernel

open Test_helpers

let%expect_test "interpret_incomplete_hole_as_constant" =
  let template = ":[B :[A]" in
  parse_template Matchers.Metasyntax.default_metasyntax template |> print_string;
  [%expect_exact {|((Constant :) (Constant "[B ") (Hole ((variable A) (pattern :[A]))))|}];

  let template = ":[B :[A~x]" in
  parse_template Matchers.Metasyntax.default_metasyntax template |> print_string;
  [%expect_exact {|((Constant :) (Constant "[B ") (Hole ((variable A) (pattern :[A~x]))))|}]

let%expect_test "interpret_incomplete_hole_as_constant_metasyntax" =
  let template = "$:x $B:x  $A" in
  let metasyntax =
    Matchers.Metasyntax.{
      syntax =
        [ Hole (Everything, Delimited (Some "$", None))
        ; Hole (Alphanum, Delimited (Some "$$", None))
        ; Regex ("$", ':', " ")
        ]
    ; identifier = "AB"
    }
  in
  parse_template metasyntax template |> print_string;
  [%expect_exact {|((Constant $) (Constant ":x ") (Hole ((variable B) (pattern "$B:x ")))
 (Constant " ") (Hole ((variable A) (pattern $A))))|}]

let%expect_test "interpret_incomplete_hole_as_constant_metasyntax" =
  let template = "(  , ,  )" in
  let metasyntax =
    Matchers.Metasyntax.{
      syntax =
        [ Hole (Everything, Delimited (Some "NOTHING", None))
        ; Hole (Everything, Reserved_identifiers ["  "; " "])
        ]
    ; identifier = "AB"
    }
  in
  parse_template metasyntax template |> print_string;
  [%expect_exact {|((Constant "(") (Hole ((variable "  ") (pattern "  "))) (Constant ,)
 (Hole ((variable " ") (pattern " "))) (Constant ,)
 (Hole ((variable "  ") (pattern "  "))) (Constant ")"))|}]

let%expect_test "interpret_incomplete_hole_as_constant_metasyntax" =
  let template = "(..,.)" in
  let metasyntax =
    Matchers.Metasyntax.{
      syntax =
        [ Hole (Everything, Delimited (Some "NOTHING", None))
        ; Hole (Everything, Reserved_identifiers [".."; "."])
        ]
    ; identifier = "AB"
    }
  in
  parse_template metasyntax template |> print_string;
  [%expect_exact {|((Constant "(") (Hole ((variable ..) (pattern ..))) (Constant ,)
 (Hole ((variable .) (pattern .))) (Constant ")"))|}]
