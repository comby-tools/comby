module Dyck = struct
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
end

module Latex = struct
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
end

module Generic = struct
  module Syntax = struct
    include Dyck.Syntax

    let escapable_string_literals =
      [ {|"|}
      ; {|'|}
      ]

    let escape_char =
      '\\'
  end

  include Matcher.Make(Syntax)
end

module Bash = struct
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
end

module Ruby = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let raw_string_literals =
      [ ({|"|}, {|"|})
      ]

    let comment_parser =
      [ Multiline ("=begin", "=end")
      ; Until_newline "#"
      ]
  end
end

module Python = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ; ({|'''|}, {|'''|})
      ]

    let comment_parser =
      [ Until_newline "#"
      ]
  end

  include Matcher.Make(Syntax)
end

module Html = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Dyck.Syntax.user_defined_delimiters @
      [ ("<", ">")
      ]

    let comment_parser =
      [ Multiline ("<!--", "-->")
      ]
  end

  include Matcher.Make(Syntax)
end

module C = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comment_parser =
      [ Multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end

  include Matcher.Make(Syntax)
end

module Java = C

module Php = struct
  module Syntax = struct
    include C.Syntax
    open Types

    let comment_parser =
      C.Syntax.comment_parser @
      [ Until_newline "#"
      ]
  end
end

module Rust = struct
  module Syntax = struct
    include C.Syntax

    let raw_string_literals =
      [ ({|r#|}, {|#|})
      ]
  end
end

module Go = struct
  module Syntax = struct
    include C.Syntax

    let raw_string_literals =
      [ ({|`|}, {|`|})
      ]
  end

  include Matcher.Make(Syntax)
end

module Javascript = Go

module OCaml = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let raw_string_literals =
      [ ("{|", "|}")
      ]

    let comments =
      [ Multiline ("(*", "*)")
      ]
  end
end

module Haskell = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ]

    let comments =
      [ Multiline ("{-", "-}")
      ; Until_newline "--"
      ]
  end

  include Matcher.Make(Syntax)
end


module Elm = Haskell
