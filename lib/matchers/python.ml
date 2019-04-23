module Python = struct
  open Types
  include Generic.Syntax

  let raw_string_literals =
    [ ({|"""|}, {|"""|})
    ]

  let comment_parser =
    [ Until_newline "#"
    ]
end

include Matcher.Make(Python)
