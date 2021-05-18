open Core
open Comby_kernel

open Match
open Matchers

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
  let result = Rewrite.substitute ~metasyntax template environment in
  print_string result;
  [%expect_exact {|$A hello|}]
