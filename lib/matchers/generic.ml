module Syntax = struct
  (** these are nestable. strings, on the other hand, are not
      nestable without escapes *)
  let user_defined_delimiters =
    [ ("(", ")")
    ; ("{", "}")
    ; ("[", "]")
    ]

  let escapable_string_literals = []

  let escape_char =
    '\\'

  let raw_string_literals = []

  let comment_parser = MParser.zero
end

include Matcher.Make(Syntax)
