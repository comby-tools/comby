module Syntax = struct
  let user_defined_delimiters =
    [ ("(", ")")
    ; ("{", "}")
    ; ("[", "]")
    ; ({|\if|}, {|\fi|})
    ]

  let escapable_string_literals = []

  let escape_char =
    '\\'

  let raw_string_literals = []

  let comment_parser s = Parsers.Comments.percentage_newline s
end

include Matcher.Make(Syntax)
