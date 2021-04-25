open Core

open Rewriter
open Rewrite_template

let parse metasyntax template =
  let (module M) = Matchers.Metasyntax.create metasyntax in
  let module Template_parser = Make(M) in
  let tree = Template_parser.parse template in
  match tree with
  | Some tree -> Sexp.to_string_hum (sexp_of_list sexp_of_extracted tree)
  | None -> "ERROR: NO PARSE"

let%expect_test "interpret_incomplete_hole_as_constant" =
  let template = ":[B :[A]" in
  parse Matchers.Metasyntax.default_metasyntax template |> print_string;
  [%expect_exact {|((Constant :) (Constant "[B ") (Hole ((variable A) (pattern :[A]))))|}];

  let template = ":[B :[A~x]" in
  parse Matchers.Metasyntax.default_metasyntax template |> print_string;
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
  parse metasyntax template |> print_string;
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
  parse metasyntax template |> print_string;
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
  parse metasyntax template |> print_string;
  [%expect_exact {|((Constant "(") (Hole ((variable ..) (pattern ..))) (Constant ,)
 (Hole ((variable .) (pattern .))) (Constant ")"))|}]
