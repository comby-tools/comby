module Syntax = struct
  open Types
  include Generic.Syntax

  let comment_parser =
    [ Multiline ("/*", "*/")
    ; Until_newline "//"
    ]
end

include Matcher.Make(Syntax)
