open Core
open Matchers

let%expect_test "serializing user defined language" =
  let c =
    { Syntax_config.user_defined_delimiters= [("if", "fi")]
    ; Syntax_config.escapable_string_literals= []
    ; Syntax_config.escape_char= '\\'
    ; Syntax_config.raw_string_literals= [({|"|}, {|"|})]
    ; Syntax_config.comment_parser= [Until_newline "%"] }
  in
  Matchers.Syntax_config.to_yojson c |> Yojson.Safe.to_string |> print_string ;
  [%expect
    {|
    {"user_defined_delimiters":[["if","fi"]],"escapable_string_literals":[],"escape_char":"\\","raw_string_literals":[["\"","\""]],"comment_parser":[["Until_newline","%"]]}
  |}]
