module Python = struct
  include Generic.Syntax

  let escapable_string_literals =
    [ {|"|}
    ; {|'|}
    ]

  let escape_char =
    '\\'

  let raw_string_literals =
    [ ({|"""|}, {|"""|})
    ]

  let comment_parser s =
    Parsers.Comments.python_newline s
end

include Matcher.Make(Python)
