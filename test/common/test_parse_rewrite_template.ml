open Core
open Comby_kernel

open Test_helpers


let%expect_test "get_offsets_for_holes" =
  let module Template_parser = Matchers.Template.Make(Matchers.Metasyntax.Default) in
  let rewrite_template = {|1234:[1]1234:[2]|} in
  let variables = Template_parser.variables rewrite_template in
  print_s [%message (variables : Matchers.Template.syntax list)];
  [%expect {|
    (variables
     (((variable 1) (pattern :[1]) (offset 4))
      ((variable 2) (pattern :[2]) (offset 12)))) |}]

let%expect_test "interpret_regex_shorthand" =
  let module Template_parser = Matchers.Template.Make(Matchers.Metasyntax.Default) in
  let rewrite_template = {|a:[~x]b|} in
  let variables = Template_parser.variables rewrite_template in
  print_s [%message (variables : Matchers.Template.syntax list)];
  [%expect {|
    (variables (((variable "") (pattern :[~x]) (offset 1)))) |}]

let%expect_test "interpret_incomplete_hole_as_constant" =
  let template = ":[B :[A]" in
  parse_template Matchers.Metasyntax.default_metasyntax template |> print_string;
  [%expect_exact {|((Constant :) (Constant "[B ")
 (Hole ((variable A) (pattern :[A]) (offset 4))))|}];

  let template = ":[B :[A~x]" in
  parse_template Matchers.Metasyntax.default_metasyntax template |> print_string;
  [%expect_exact {|((Constant :) (Constant "[B ")
 (Hole ((variable A) (pattern :[A~x]) (offset 4))))|}]

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
  [%expect_exact {|((Hole ((variable "") (pattern "$:x ") (offset 0)))
 (Hole ((variable B) (pattern "$B:x ") (offset 4))) (Constant " ")
 (Hole ((variable A) (pattern $A) (offset 10))))|}]

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
  [%expect_exact {|((Constant "(") (Hole ((variable "  ") (pattern "  ") (offset 1)))
 (Constant ,) (Hole ((variable " ") (pattern " ") (offset 4))) (Constant ,)
 (Hole ((variable "  ") (pattern "  ") (offset 6))) (Constant ")"))|}]

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
  [%expect_exact {|((Constant "(") (Hole ((variable ..) (pattern ..) (offset 1))) (Constant ,)
 (Hole ((variable .) (pattern .) (offset 4))) (Constant ")"))|}]

let%expect_test "parse_reserved_identifiers_as_holes" =
  let template = "(α)" in
  let metasyntax =
    Matchers.Metasyntax.{
      syntax =
        [ Hole (Expression, Reserved_identifiers ["α"])
        ]
    ; identifier = "AB"
    }
  in
  parse_template metasyntax template |> print_string;
  [%expect_exact {|((Constant "(")
 (Hole ((variable "\206\177") (pattern "\206\177") (offset 1)))
 (Constant ")"))|}]
