open MParser

module Syntax = struct
  include Generic.Syntax

  let escapable_string_literals =
    [ {|"|}
    ; {|'|}
    ]

  let escape_char =
    '\\'

  let raw_string_literals =
    []

  let comment_parser s =
    (Parsers.Comments.c_multiline
     <|> Parsers.Comments.c_newline) s
end

include Matcher.Make(Syntax)
