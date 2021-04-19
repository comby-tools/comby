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
        [ Hole (Everything, Delimited (Some "$", None))
        ; Hole (Alphanum, Delimited (Some "$$", None))
        ; Regex ("$", ':', " ")
        ]
    ; identifier = "AB"
    }
  in
  (* Don't just substitute for `$B`, but for `$B:\w+ `. This depends on Regex (more specific syntax) being defined _after_ the general syntax. *)
  let template = {|$A $B:\w+ |} in
  let environment = Environment.add (Environment.create ()) "B" "hello" in
  let result, _ = Rewrite_template.substitute ~metasyntax template environment in
  print_string result;
  [%expect_exact {|$A hello|}]
