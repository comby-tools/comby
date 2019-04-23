module Syntax = struct
  open Types
  include Dyck.Syntax

  let user_defined_delimiters =
    Dyck.Syntax.user_defined_delimiters @
    [ ({|\if|}, {|\fi|})
    ]

  let comment_parser =
    [ Until_newline "%"
    ]
end

include Matcher.Make(Syntax)
