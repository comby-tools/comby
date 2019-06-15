let experimental = false

module Text = struct
  module Syntax = struct
    let user_defined_delimiters = []
    let escapable_string_literals = []

    let escape_char =
      '\\'

    let raw_string_literals = []

    let comment_parser = []
  end

  include Matcher.Make(Syntax)
end

module Paren = struct
  module Syntax = struct
    include Text.Syntax
    let user_defined_delimiters =
      [ "(", ")"
      ]
  end
  include Matcher.Make(Syntax)
end

module Dyck = struct
  module Syntax = struct
    let user_defined_delimiters =
      [ "(", ")"
      ; "{", "}"
      ; "[", "]"
      ]

    let escapable_string_literals = []

    (* This is ignored since there are no literals *)
    let escape_char =
      '\\'

    let raw_string_literals = []

    let comment_parser = []
  end

  include Matcher.Make(Syntax)
end

module Json = Dyck

module Latex = struct
  module Syntax = struct
    open Types
    include Dyck.Syntax

    let user_defined_delimiters =
      Dyck.Syntax.user_defined_delimiters @
      [ {|\if|}, {|\fi|}
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

module Clojure = struct
  module Syntax = struct
    open Types
    include Dyck.Syntax

    let escapable_string_literals =
      [ {|"|}
      ]

    let comment_parser =
      [ Until_newline ";"
      ]
  end

  include Matcher.Make(Syntax)
end

module Lisp = struct
  module Syntax = struct
    include Clojure.Syntax
    open Types

    let comment_parser =
      Clojure.Syntax.comment_parser @
      [ Nested_multiline ("#|", "|#")
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
      [ "if ", "fi"
      ; "case ", "esac"
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

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @ if experimental then
        [ "class", "end"
        ; "def", "end"
        ; "do", "end"
        ; "if", "end"
        ; "case", "end"
        ; "unless", "end"
        ; "while", "end"
        ; "until", "end"
        ; "for", "end"
        ; "begin", "end"
        ; "module", "end"
        ]
      else
        []

    let raw_string_literals =
      [ ({|"|}, {|"|})
      ]

    let comment_parser =
      [ Multiline ("=begin", "=end")
      ; Until_newline "#"
      ]
  end

  include Matcher.Make(Syntax)
end

module Elixir = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @ if experimental then
        [ "fn", "end"
        ; "do", "end"
        ; "case", "end"
        ; "cond", "end"
        ; "if", "end"
        ; "<", ">"
        ]
      else
        []

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
      [ "<", ">"
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

module Erlang = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @ if experimental then
        [ "fun", "end"
        ; "case", "end"
        ; "if", "end"
        ]
      else
        []

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

module Csharp = C

module Java = C

module CSS = C

module Kotlin = C

module Scala = C

module Dart = struct
  module Syntax = struct
    include C.Syntax

    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ; ({|'''|}, {|'''|})
      ]
  end

  include Matcher.Make(Syntax)
end

module Php = struct
  module Syntax = struct
    include C.Syntax
    open Types

    let comment_parser =
      C.Syntax.comment_parser @
      [ Until_newline "#"
      ]
  end

  include Matcher.Make(Syntax)
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

module Swift = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comment_parser =
      [ Nested_multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end

  include Matcher.Make(Syntax)
end

module Rust = struct
  module Syntax = struct
    include Swift.Syntax

    (* Override ' as escapable string literal, since
       these can be used in typing *)
    let escapable_string_literals =
      [ {|"|}
      ]

    let raw_string_literals =
      [ ({|r#|}, {|#|})
      ]
  end

  include Matcher.Make(Syntax)
end

module OCaml = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @ if experimental then
        [ "begin", "end"
        ; "struct", "end"
        ; "sig", "end"
        ]
      else
        []


    (* Override ' as escapable string literal, since
       these can be used in typing *)
    let escapable_string_literals =
      [ {|"|}
      ]

    let raw_string_literals =
      [ ("{|", "|}")
      ]

    let comments =
      [ Nested_multiline ("(*", "*)")
      ]
  end

  include Matcher.Make(Syntax)
end

module Fsharp = OCaml

(** Follow Free Pascal that allows nested comments, although Rosetta takes the opposite view. *)
module Pascal = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("(*", "*)")
      ; Nested_multiline ("{", "}")
      ; Until_newline "//"
      ]
  end

  include Matcher.Make(Syntax)
end

module Julia = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("#=", "=#")
      ; Until_newline "#"
      ]
  end

  include Matcher.Make(Syntax)
end

module Fortran = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comments =
      [ Until_newline "!"
      ]
  end

  include Matcher.Make(Syntax)
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

(** For testing *)
module C_nested_comments = struct
  module Syntax = struct
    open Types
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("/*", "*/")
      ]
  end

  include Matcher.Make(Syntax)
end

let select_with_extension extension : (module Types.Matcher.S) =
  match extension with
  | ".c" | ".h" | ".cc" | ".cpp" | ".hpp" -> (module C)
  | ".clj" -> (module Clojure)
  | ".cs" -> (module Csharp)
  | ".css" -> (module CSS)
  | ".dart" -> (module Dart)
  | ".dyck" -> (module Dyck)
  | ".elm" -> (module Elm)
  | ".erl" -> (module Erlang)
  | ".ex" -> (module Elixir)
  | ".f" | ".for" | ".f90"
  | ".f95" | ".f03" | ".f08" | ".F" | ".F90" -> (module Fortran)
  | ".fsx" -> (module Fsharp)
  | ".html" | ".xml" -> (module Html)
  | ".hs" -> (module Haskell)
  | ".go" -> (module Go)
  | ".java" -> (module Java)
  | ".jl" -> (module Julia)
  | ".js" | ".ts" -> (module Javascript)
  | ".json" -> (module Json)
  | ".ml" | ".mli" -> (module OCaml)
  | ".paren" -> (module Paren)
  | ".pas" -> (module Pascal)
  | ".php" -> (module Php)
  | ".py" -> (module Python)
  | ".rb" -> (module Ruby)
  | ".rs" -> (module Rust)
  | ".s" | ".asm" -> (module Assembly)
  | ".scala" -> (module Scala)
  | ".sql" -> (module SQL)
  | ".sh" -> (module Bash)
  | ".swift" -> (module Swift)
  | ".tex" | ".bib" -> (module Latex)
  | ".txt" -> (module Text)
  | _ -> (module Generic)
