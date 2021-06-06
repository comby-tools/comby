open Core
open Comby_kernel

open Test_helpers

open Matchers

let%expect_test "get_offsets_for_holes" =
  let module Template_parser = Template.Make(Metasyntax.Default)(External.Default) in
  let rewrite_template = {|1234:[1]1234:[2]|} in
  let variables = Template_parser.variables rewrite_template in
  print_s [%message (variables : Template.syntax list)];
  [%expect {|
    (variables
     (((variable 1) (pattern :[1]) (offset 4) (kind Value))
      ((variable 2) (pattern :[2]) (offset 12) (kind Value)))) |}]

let%expect_test "interpret_regex_shorthand" =
  let module Template_parser = Template.Make(Metasyntax.Default)(External.Default) in
  let rewrite_template = {|a:[~x]b|} in
  let variables = Template_parser.variables rewrite_template in
  print_s [%message (variables : Template.syntax list)];
  [%expect {|
    (variables (((variable "") (pattern :[~x]) (offset 1) (kind Value)))) |}]

let%expect_test "interpret_incomplete_hole_as_constant" =
  let template = ":[B :[A]" in
  parse_template Metasyntax.default_metasyntax template |> print_string;
  [%expect_exact {|((Constant ":[B ")
 (Hole ((variable A) (pattern :[A]) (offset 4) (kind Value))))|}];

  let template = ":[B :[A~x]" in
  parse_template Metasyntax.default_metasyntax template |> print_string;
  [%expect_exact {|((Constant ":[B ")
 (Hole ((variable A) (pattern :[A~x]) (offset 4) (kind Value))))|}]

let%expect_test "interpret_incomplete_hole_as_constant_metasyntax" =
  let template = "$:x $B:x  $A" in
  let metasyntax =
    Metasyntax.{
      syntax =
        [ Hole (Everything, Delimited (Some "$", None))
        ; Hole (Alphanum, Delimited (Some "$$", None))
        ; Regex ("$", ':', " ")
        ]
    ; identifier = "AB"
    ; aliases = []
    }
  in
  parse_template metasyntax template |> print_string;
  [%expect_exact {|((Hole ((variable "") (pattern "$:x ") (offset 0) (kind Value)))
 (Hole ((variable B) (pattern "$B:x ") (offset 4) (kind Value)))
 (Constant " ") (Hole ((variable A) (pattern $A) (offset 10) (kind Value))))|}]

let%expect_test "interpret_incomplete_hole_as_constant_metasyntax" =
  let template = "(  , ,  )" in
  let metasyntax =
    Metasyntax.{
      syntax =
        [ Hole (Everything, Delimited (Some "NOTHING", None))
        ; Hole (Everything, Reserved_identifiers ["  "; " "])
        ]
    ; identifier = "AB"
    ; aliases = []
    }
  in
  parse_template metasyntax template |> print_string;
  [%expect_exact {|((Constant "(")
 (Hole ((variable "  ") (pattern "  ") (offset 1) (kind Value))) (Constant ,)
 (Hole ((variable " ") (pattern " ") (offset 4) (kind Value))) (Constant ,)
 (Hole ((variable "  ") (pattern "  ") (offset 6) (kind Value)))
 (Constant ")"))|}]

let%expect_test "interpret_incomplete_hole_as_constant_metasyntax" =
  let template = "(..,.)" in
  let metasyntax =
    Metasyntax.{
      syntax =
        [ Hole (Everything, Delimited (Some "NOTHING", None))
        ; Hole (Everything, Reserved_identifiers [".."; "."])
        ]
    ; identifier = "AB"
    ; aliases = []
    }
  in
  parse_template metasyntax template |> print_string;
  [%expect_exact {|((Constant "(") (Hole ((variable ..) (pattern ..) (offset 1) (kind Value)))
 (Constant ,) (Hole ((variable .) (pattern .) (offset 4) (kind Value)))
 (Constant ")"))|}]

let%expect_test "parse_reserved_identifiers_as_holes" =
  let template = "(α)" in
  let metasyntax =
    Metasyntax.{
      syntax =
        [ Hole (Expression, Reserved_identifiers ["α"])
        ]
    ; identifier = "AB"
    ; aliases = []
    }
  in
  parse_template metasyntax template |> print_string;
  [%expect_exact {|((Constant "(")
 (Hole ((variable "\206\177") (pattern "\206\177") (offset 1) (kind Value)))
 (Constant ")"))|}]

let%expect_test "get_offsets_for_holes" =
  let module Template_parser = Template.Make(Metasyntax.Default)(External.Default) in
  let template = ":[a].type :[b].length :[[c]].lengtha :[d.].length.ok (:[e].length)" in
  let variables = Template_parser.variables template in
  print_s [%message (variables : Template.syntax list)];
  [%expect {|
    (variables
     (((variable a) (pattern :[a]) (offset 0) (kind Value))
      ((variable b) (pattern :[b].length) (offset 10) (kind Length))
      ((variable c) (pattern :[[c]]) (offset 22) (kind Value))
      ((variable d) (pattern :[d.].length) (offset 37) (kind Length))
      ((variable e) (pattern :[e].length) (offset 54) (kind Length)))) |}]
