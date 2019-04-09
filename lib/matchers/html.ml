module Syntax = struct
  include Generic.Syntax

  let user_defined_delimiters =
    Generic.Syntax.user_defined_delimiters @
    [ ("<", ">")
    ]


  let escapable_string_literals =
    [ {|"|}
    ; {|'|}
    ]

end

include Matcher.Make(Syntax)
