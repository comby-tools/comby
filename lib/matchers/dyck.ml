module Syntax = struct
  let user_defined_delimiters =
    [ ("(", ")")
    ; ("{", "}")
    ; ("[", "]")
    ]

  let escapable_string_literals = []

  let escape_char =
    '\\'

  let raw_string_literals = []

  let comment_parser = []
end

include Matcher.Make(Syntax)
