module Syntax = struct
  open Types

  let user_defined_delimiters =
    Dyck.Syntax.user_defined_delimiters @
    [ ("<", ">")
    ]

  let escapable_string_literals =
    [ {|"|}
    ; {|'|}
    ]

  let raw_string_literals = []

  let escape_char =
    '\\'

  let comment_parser =
    [ Multiline ("<!--", "-->")
    ]
end

include Matcher.Make(Syntax)
