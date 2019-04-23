module Syntax = struct
  include C.Syntax

  let raw_string_literals =
    [ ({|`|}, {|`|})
    ]
end

include Matcher.Make(Syntax)
