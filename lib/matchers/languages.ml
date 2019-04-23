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

module Assembly = struct
  module Syntax = struct
    open Types
    include Dyck.Syntax

    let comment_parser =
      [ Until_newline ";"
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

module Elixir = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ]

    let comment_parser =
      [ Until_newline "#"
      ]
  end

  include Matcher.Make(Syntax)
end


module Python = struct
  module Syntax = struct
    include Elixir.Syntax

    let raw_string_literals =
      Elixir.Syntax.raw_string_literals @
      [ ({|'''|}, {|'''|})
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

module SQL = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comment_parser =
      [ Multiline ("/*", "*/")
      ; Until_newline "--"
      ]
  end

  include Matcher.Make(Syntax)
end

module Clojure = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comment_parser =
      [ Until_newline ";"
      ]
  end

  include Matcher.Make(Syntax)
end

module Erlang = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comment_parser =
      [ Until_newline "%"
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

module CSS = C

module Kotlin = C

module Scala = C

module Dart = C

(* TODO(RVT): model nested comments *)
module Swift = C

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

(* TODO(RVT): model nested comments *)
module Lisp = struct
  module Syntax = struct
    include C.Syntax
    open Types

    let comment_parser =
      C.Syntax.comment_parser @
      [ Until_newline ";"
      ; Multiline ("#|", "|#")
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

(* TODO(RVT): model nested comments *)
module Rust = struct
  module Syntax = struct
    include C.Syntax

    let raw_string_literals =
      [ ({|r#|}, {|#|})
      ]
  end
end

(* TODO(RVT): model nested comments *)
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
