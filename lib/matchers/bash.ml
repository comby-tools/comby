module Syntax = struct
  open Types
  include Generic.Syntax

  let user_defined_delimiters =
    Dyck.Syntax.user_defined_delimiters @
    [ ("if", "fi")
    ; ("case", "esac")
    ]

  let comment_parser =
    [ Until_newline "#" ]
end

include Matcher.Make(Syntax)
