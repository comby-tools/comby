open Core
open Syntax_config

module Text = struct
  module Info = struct
    let name = "Text"
    let extensions = [".txt"]
  end

  module Syntax = struct
    let user_defined_delimiters = []
    let escapable_string_literals = []

    let escape_char =
      '\\'

    let raw_string_literals = []

    let comment_parser = []
  end

  include Matcher.Make (Syntax) (Info)
end

module Paren = struct
  module Info = struct
    let name = "Paren"
    let extensions = [".paren"]
  end

  module Syntax = struct
    include Text.Syntax
    let user_defined_delimiters =
      [ "(", ")"
      ]
  end
  include Matcher.Make (Syntax) (Info)
end

module Dyck = struct
  module Info = struct
    let name = "Dyck"
    let extensions = [".dyck"]
  end

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

  include Matcher.Make (Syntax) (Info)
end

module Json = struct
  module Info = struct
    let name = "JSON"
    let extensions = [".json"]
  end

  include Matcher.Make (Dyck.Syntax) (Info)
end

module Latex = struct
  module Info = struct
    let name = "LaTeX"
    let extensions = [".tex"; ".bib"]
  end

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

  include Matcher.Make (Syntax) (Info)
end

module Assembly = struct
  module Info = struct
    let name = "Assembly"
    let extensions = [".s"; ".asm"]
  end

  module Syntax = struct
    open Types
    include Dyck.Syntax

    let comment_parser =
      [ Until_newline ";"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Clojure = struct
  module Info = struct
    let name = "Clojure"
    let extensions = [".clj"]
  end

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

  include Matcher.Make (Syntax) (Info)
end

module Lisp = struct
  module Info = struct
    let name = "Lisp"
    let extensions = [".lisp"]
  end

  module Syntax = struct
    include Clojure.Syntax
    open Types

    let comment_parser =
      Clojure.Syntax.comment_parser @
      [ Nested_multiline ("#|", "|#")
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Generic = struct
  module Info = struct
    let name = "Generic"
    let extensions = [".generic"]
  end

  module Syntax = struct
    include Dyck.Syntax

    let escapable_string_literals =
      [ {|"|}
      ; {|'|}
      ]

    let escape_char =
      '\\'
  end

  include Matcher.Make (Syntax) (Info)
end

module Bash = struct
  module Info = struct
    let name = "Bash"
    let extensions = [".sh"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Dyck.Syntax.user_defined_delimiters @
      [ "if", "fi"
      ; "case", "esac"
      ; "for", "done"
      ; "until", "done"
      ; "while", "done"
      ]

    let comment_parser =
      [ Until_newline "#" ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Ruby = struct
  module Info = struct
    let name = "Ruby"
    let extensions = [".rb"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @
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

    let raw_string_literals =
      [ ({|"|}, {|"|})
      ]

    let comment_parser =
      [ Multiline ("=begin", "=end")
      ; Until_newline "#"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Elixir = struct
  module Info = struct
    let name = "Elixir"
    let extensions = [".ex"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @
      [ "fn", "end"
      ; "do", "end"
      ; "case", "end"
      ; "cond", "end"
      ; "if", "end"
      ; "<", ">"
      ]

    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ]

    let comment_parser =
      [ Until_newline "#"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end


module Python = struct
  module Info = struct
    let name = "Python"
    let extensions = [".py"; ".pyi"]
  end

  module Syntax = struct
    include Elixir.Syntax

    let raw_string_literals =
      Elixir.Syntax.raw_string_literals @
      [ ({|'''|}, {|'''|})
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Html = struct
  module Info = struct
    let name = "HTML"
    let extensions = [".html"]
  end

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

  include Matcher.Make (Syntax) (Info)
end

module Xml = struct
  module Info = struct
    let name = "XML"
    let extensions = [".xml"]
  end

  include Matcher.Make (Html.Syntax) (Info)
end

module SQL = struct
  module Info = struct
    let name = "SQL"
    let extensions = [".sql"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let comment_parser =
      [ Multiline ("/*", "*/")
      ; Until_newline "--"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Erlang = struct
  module Info = struct
    let name = "Erlang"
    let extensions = [".erl"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @
      [ "fun", "end"
      ; "case", "end"
      ; "if", "end"
      ]

    let comment_parser =
      [ Until_newline "%"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module C = struct
  module Info = struct
    let name = "C"
    let extensions =
      [ ".c"
      ; ".h"
      ; ".cc"
      ; ".cpp"
      ; ".hpp"
      ]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let comment_parser =
      [ Multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Csharp = struct
  module Info = struct
    let name = "C#"
    let extensions = [".cs"]
  end

  include Matcher.Make (C.Syntax) (Info)
end

module Java = struct
  module Info = struct
    let name = "Java"
    let extensions = [".java"]
  end

  include Matcher.Make (C.Syntax) (Info)
end

module CSS = struct
  module Info = struct
    let name = "CSS"
    let extensions = [".css"]
  end

  include Matcher.Make (C.Syntax) (Info)
end

module Kotlin = struct
  module Info = struct
    let name = "Kotlin"
    let extensions = [".kt"; ".kts"]
  end

  include Matcher.Make (C.Syntax) (Info)
end

module Scala = struct
  module Info = struct
    let name = "Scala"
    let extensions = [".scala"]
  end

  include Matcher.Make (C.Syntax) (Info)
end

module Dart = struct
  module Info = struct
    let name = "Dart"
    let extensions = [".dart"]
  end

  module Syntax = struct
    include C.Syntax

    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ; ({|'''|}, {|'''|})
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Php = struct
  module Info = struct
    let name = "PHP"
    let extensions = [".php"]
  end

  module Syntax = struct
    include C.Syntax
    open Types

    let comment_parser =
      C.Syntax.comment_parser @
      [ Until_newline "#"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Go = struct
  module Info = struct
    let name = "Go"
    let extensions = [".go"]
  end

  module Syntax = struct
    include C.Syntax

    let raw_string_literals =
      [ ({|`|}, {|`|})
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Javascript = struct
  module Info = struct
    let name = "Javascript/Typescript"
    let extensions = [".js"; ".ts"]
  end

  include Matcher.Make (Go.Syntax) (Info)
end

module Swift = struct
  module Info = struct
    let name = "Swift"
    let extensions = [".swift"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let comment_parser =
      [ Nested_multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Rust = struct
  module Info = struct
    let name = "Rust"
    let extensions = [".rs"]
  end

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

  include Matcher.Make (Syntax) (Info)
end

module OCaml = struct
  module Info = struct
    let name = "OCaml"
    let extensions = [".ml"; ".mli"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @
      [ "begin", "end"
      ; "struct", "end"
      ; "sig", "end"
      ]


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

  include Matcher.Make (Syntax) (Info)
end

module Fsharp = struct
  module Info = struct
    let name = "F#"
    let extensions = [".fsx"]
  end

  include Matcher.Make (OCaml.Syntax) (Info)
end

(** Follow Free Pascal that allows nested comments, although Rosetta takes the opposite view. *)
module Pascal = struct
  module Info = struct
    let name = "Pascal"
    let extensions = [".pas"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("(*", "*)")
      ; Nested_multiline ("{", "}")
      ; Until_newline "//"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Julia = struct
  module Info = struct
    let name = "Julia"
    let extensions = [".jl"]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("#=", "=#")
      ; Until_newline "#"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Fortran = struct
  module Info = struct
    let name = "Fortran"
    let extensions =
      [ ".f"
      ; ".for"
      ; ".f90"
      ; ".f95"
      ; ".f03"
      ; ".f08"
      ; ".F"
      ; ".F90"
      ]
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let comments =
      [ Until_newline "!"
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

module Haskell = struct
  module Info = struct
    let name = "Haskell"
    let extensions = [".hs"]
  end

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

  include Matcher.Make (Syntax) (Info)
end

module Elm = struct
  module Info = struct
    let name = "Elm"
    let extensions = [".elm"]
  end

  include Matcher.Make (Haskell.Syntax) (Info)
end

(** For testing *)
module C_nested_comments = struct
  module Info = struct
    let name = "C_with_nested_comments"
    let extensions = []
  end

  module Syntax = struct
    open Types
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("/*", "*/")
      ]
  end

  include Matcher.Make (Syntax) (Info)
end

let all : (module Types.Matcher.S) list =
  [ (module Assembly)
  ; (module Bash)
  ; (module C)
  ; (module Csharp)
  ; (module CSS)
  ; (module Dart)
  ; (module Dyck)
  ; (module Clojure)
  ; (module Elm)
  ; (module Erlang)
  ; (module Elixir)
  ; (module Fortran)
  ; (module Fsharp)
  ; (module Html)
  ; (module Haskell)
  ; (module Go)
  ; (module Java)
  ; (module Javascript)
  ; (module Json)
  ; (module Julia)
  ; (module Latex)
  ; (module OCaml)
  ; (module Paren)
  ; (module Pascal)
  ; (module Php)
  ; (module Python)
  ; (module Ruby)
  ; (module Rust)
  ; (module Scala)
  ; (module SQL)
  ; (module Swift)
  ; (module Text)
  ; (module Xml)
  ; (module Generic)
  ]

let select_with_extension extension : (module Types.Matcher.S) =
  List.find all ~f:(fun (module M) -> List.exists M.extensions ~f:((=) extension))
  |> function
  | Some matcher -> matcher
  | None -> (module Generic)

let create
    { user_defined_delimiters
    ; escapable_string_literals
    ; escape_char
    ; raw_string_literals
    ; comment_parser
    } =
  let module Info = struct
    let name = "User_defined_language"
    let extensions = []
  end
  in
  let module User_language = struct
    let user_defined_delimiters = user_defined_delimiters
    let escapable_string_literals = escapable_string_literals
    let escape_char = escape_char
    let raw_string_literals = raw_string_literals
    let comment_parser = comment_parser
  end
  in
  (module Matcher.Make (User_language) (Info) : Types.Matcher.S)
