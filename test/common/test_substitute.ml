open Core

open Match
open Rewriter
open Rewrite_template

let parse metasyntax template =
  let (module M) = Matchers.Metasyntax.create metasyntax in
  let module Template_parser = Make(M) in
  let tree = Template_parser.parse template in
  match tree with
  | Some tree -> Sexp.to_string_hum (sexp_of_list sexp_of_extracted tree)
  | None -> "ERROR: NO PARSE"

let%expect_test "substitute_entire_regex_pattern_in_custom_metasyntax" =
  let metasyntax =
    Matchers.Metasyntax.{
      syntax =
        [ Regex ("$", ':', " ")
        ; Hole (Everything, Delimited (Some "$", None))
        ; Hole (Alphanum, Delimited (Some "$$", None))
        ]
    ; identifier = "AB"
    }
  in
  let template = {|$A $B:\w+ |} in (* Don't just substitute for `$B`, but for `$B:\w+ ` *)
  let environment = Environment.add (Environment.create ()) "B" "hello" in
  let result, _ = Rewrite_template.substitute ~metasyntax template environment in
  print_string result;
  [%expect_exact {|$A hello|}]

(*
let%expect_test "offsets_for_holes" =
  let metasyntax =
    Matchers.Metasyntax.{
      syntax =
        [ Regex ("$", ':', " ")
        ; Hole (Everything, Delimited (Some "$", None))
        ; Hole (Alphanum, Delimited (Some "$$", None))
        ]
    ; identifier = "AB"
    }
  in
  let template = {|$A $B:\w+ |} in
  let environment = Environment.add (Environment.create ()) "B" "hello" in
  let result, _ = Rewrite_template.get_offsets_for_holes ~metasyntax template environment in
  print_string result;
  [%expect_exact {|$A hello|}];
*)
