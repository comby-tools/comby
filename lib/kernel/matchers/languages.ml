open Core_kernel

open Types.Syntax

let ordinary_string = Some { delimiters = [{|"|}]; escape_character = '\\' }

module Text = struct
  module Info = struct
    let name = "Text"
    let extensions = [".txt"; ".md"; ".rst"; ".org"]
  end

  module Syntax = struct
    let user_defined_delimiters = []
    let escapable_string_literals = None
    let raw_string_literals = []
    let comments = []
  end
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

    let escapable_string_literals = None

    let raw_string_literals = []

    let comments = []
  end
end

module Latex = struct
  module Info = struct
    let name = "LaTeX"
    let extensions = [".tex"; ".bib"]
  end

  module Syntax = struct
    include Dyck.Syntax

    let user_defined_delimiters =
      Dyck.Syntax.user_defined_delimiters @
      [ {|\if|}, {|\fi|}
      ]

    let comments =
      [ Until_newline "%"
      ]
  end
end

module Assembly = struct
  module Info = struct
    let name = "Assembly"
    let extensions = [".s"; ".asm"]
  end

  module Syntax = struct
    include Dyck.Syntax

    let comments =
      [ Until_newline ";"
      ]
  end
end

module Clojure = struct
  module Info = struct
    let name = "Clojure"
    let extensions = [".clj"]
  end

  module Syntax = struct
    include Dyck.Syntax

    let escapable_string_literals = ordinary_string

    let comments =
      [ Until_newline ";"
      ]
  end
end

module Lisp = struct
  module Info = struct
    let name = "Lisp"
    let extensions = [".lisp"]
  end

  module Syntax = struct
    include Clojure.Syntax

    let comments =
      Clojure.Syntax.comments @
      [ Nested_multiline ("#|", "|#")
      ]
  end
end

module Generic = struct
  module Info = struct
    let name = "Generic"
    let extensions = [".generic"]
  end

  module Syntax = struct
    include Dyck.Syntax

    let escapable_string_literals =
      Some
        { delimiters = [{|"|}; {|'|}]
        ; escape_character = '\\'
        }
  end
end

module JSON = struct
  module Info = struct
    let name = "JSON"
    let extensions = [".json"]
  end

  module Syntax = Generic.Syntax
end

module JSONC = struct
  module Info = struct
    let name = "JSONC"
    let extensions = [".jsonc"]
  end

  module Syntax = struct
    include Generic.Syntax
    let comments =
      [ Multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end
end

module GraphQL = struct
  module Info = struct
    let name = "GraphQL"
    let extensions = [".gql"; ".graphql"]
  end

  module Syntax = struct
    include Generic.Syntax

    let comments =
      [ Until_newline "#"
      ]
  end
end

module Dhall = struct
  module Info = struct
    let name = "Dhall"
    let extensions = [".dhall"]
  end

  module Syntax = struct
    include Generic.Syntax

    let raw_string_literals =
      [ ("''", "''")
      ; ("`", "`") ]

    let comments =
      [ Until_newline "--"
      ; Nested_multiline ("{-", "-}")
      ]
  end
end

module Bash = struct
  module Info = struct
    let name = "Bash"
    let extensions = [".sh"]
  end

  module Syntax = struct
    include Generic.Syntax

    let user_defined_delimiters =
      Dyck.Syntax.user_defined_delimiters @
      [ "if", "fi"
      ; "case", "esac"
      ; "for", "done"
      ; "until", "done"
      ; "while", "done"
      ]

    let comments =
      [ Until_newline "#" ]
  end
end

module Ruby = struct
  module Info = struct
    let name = "Ruby"
    let extensions = [".rb"]
  end

  module Syntax = struct
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

    let comments =
      [ Multiline ("=begin", "=end")
      ; Until_newline "#"
      ]
  end
end

module Elixir = struct
  module Info = struct
    let name = "Elixir"
    let extensions = [".ex"]
  end

  module Syntax = struct
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @ [ "fn", "end"
        ; "do", "end"
        ]

    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ]

    let comments =
      [ Until_newline "#"
      ]
  end
end


module Python = struct
  module Info = struct
    let name = "Python"
    let extensions = [".py"; ".pyi"]
  end

  module Syntax = struct
    include Generic.Syntax

    let raw_string_literals =
      [ ({|'''|}, {|'''|})
      ; ({|"""|}, {|"""|})
      ]

    let comments =
      [ Until_newline "#"
      ]
  end
end

module Html = struct
  module Info = struct
    let name = "HTML"
    let extensions = [".html"]
  end

  module Syntax = struct
    include Generic.Syntax

    let user_defined_delimiters =
      Dyck.Syntax.user_defined_delimiters @
      [ "<", ">"
      ]

    let comments =
      [ Multiline ("<!--", "-->")
      ]
  end
end

module Xml = struct
  module Info = struct
    let name = "XML"
    let extensions = [".xml"]
  end

  module Syntax = Html.Syntax
end

module SQL = struct
  module Info = struct
    let name = "SQL"
    let extensions = [".sql"]
  end

  module Syntax = struct
    include Generic.Syntax

    let comments =
      [ Multiline ("/*", "*/")
      ; Until_newline "--"
      ]
  end
end

module Erlang = struct
  module Info = struct
    let name = "Erlang"
    let extensions = [".erl"]
  end

  module Syntax = struct
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @
      [ "fun", "end"
      ; "case", "end"
      ; "if", "end"
      ]

    let comments =
      [ Until_newline "%"
      ]
  end
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
    include Generic.Syntax

    let comments =
      [ Multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end
end

module Csharp = struct
  module Info = struct
    let name = "C#"
    let extensions = [".cs"]
  end

  module Syntax = C.Syntax
end

module Java = struct
  module Info = struct
    let name = "Java"
    let extensions = [".java"]
  end

  module Syntax = C.Syntax
end

module CSS = struct
  module Info = struct
    let name = "CSS"
    let extensions = [".css"]
  end

  module Syntax = C.Syntax
end

module Kotlin = struct
  module Info = struct
    let name = "Kotlin"
    let extensions = [".kt"; ".kts"]
  end

  module Syntax = C.Syntax
end

module Scala = struct
  module Info = struct
    let name = "Scala"
    let extensions = [".scala"]
  end

  module Syntax = C.Syntax
end

module Nim = struct
  module Info = struct
    let name = "Nim"
    let extensions = [".nim"]
  end

  module Syntax = struct
    include Dyck.Syntax


    (* Excludes ' as escapable string literal, since these can be used in
       as type suffixes like 'i8 *)
    let escapable_string_literals = ordinary_string

    (* Not supported: "raw" string literals as defined in https://nim-lang.org/docs/manual.html#lexical-analysis-raw-string-literals where r"a""b" means the two "" are escaped to single *)
    (* Not supported: generalized raw string literals as in https://nim-lang.org/docs/manual.html#lexical-analysis-raw-string-literals that needs more special casing in the lexer *)
    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ]

    let comments =
      [ Until_newline "#"
      ; Nested_multiline ("#[", "]#")
      ]
  end
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
end

module Php = struct
  module Info = struct
    let name = "PHP"
    let extensions = [".php"]
  end

  module Syntax = struct
    include C.Syntax

    let comments =
      C.Syntax.comments @
      [ Until_newline "#"
      ]
  end
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
end

module Solidity = struct
  module Info = struct
    let name = "Solidity"
    let extensions = [".sol"]
  end

  (* Note this doesn't take care of multiple concatenated strings:
     https://github.com/ethereum/solidity/issues/7292 *)
  module Syntax = C.Syntax
end


module Javascript = struct
  module Info = struct
    let name = "JavaScript"
    let extensions = [".js"]
  end

  module Syntax = struct
    include Dyck.Syntax

    let raw_string_literals =
      [ ({|`|}, {|`|})
      ]

    let escapable_string_literals =
      Some
        { delimiters = [{|"|}; {|'|}]
        ; escape_character = '\\'
        }

    let comments =
      [ Multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end
end

module Typescript = struct
  module Info = struct
    let name = "TypeScript"
    let extensions = [".ts"]
  end

  module Syntax = Javascript.Syntax
end

module Jsx = struct
  module Info = struct
    let name = "JSX"
    let extensions = [".jsx"]
  end

  module Syntax = Javascript.Syntax
end

module Tsx = struct
  module Info = struct
    let name = "TSX"
    let extensions = [".tsx"]
  end

  module Syntax = Typescript.Syntax
end

module Swift = struct
  module Info = struct
    let name = "Swift"
    let extensions = [".swift"]
  end

  module Syntax = struct
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end
end

module Rust = struct
  module Info = struct
    let name = "Rust"
    let extensions = [".rs"]
  end

  module Syntax = struct
    include Generic.Syntax

    (* Excludes ' as escapable string literal, since these can be used in
       typing. *)
    let escapable_string_literals = ordinary_string

    let raw_string_literals =
      [ {|r#|}, {|#|}
      ]

    let comments =
      [ Nested_multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end
end

module Move = struct
  module Info = struct
    let name = "Move"
    let extensions = [".move"]
  end

  module Syntax = struct
    include Generic.Syntax

    let escapable_string_literals = ordinary_string

    let comments =
      [ Nested_multiline ("/*", "*/")
      ; Until_newline "//"
      ]
  end
end

module OCaml = struct
  module Info = struct
    let name = "OCaml"
    let extensions = [".ml"; ".mli"]
  end

  module Syntax = struct
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @
      [ "begin", "end"
      ; "struct", "end"
      ; "sig", "end"
      ]


    (* Excludes ' as escapable string literal, since these can be used in
       typing. *)
    let escapable_string_literals = ordinary_string

    let raw_string_literals =
      [ ("{|", "|}")
      ]

    let comments =
      [ Nested_multiline ("(*", "*)")
      ]
  end
end

module Reason = struct
  module Info = struct
    let name = "Reason"
    let extensions = [".re"; ".rei"]
  end

  module Syntax = struct
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters

    (* Excludes ' as escapable string literal, since these can be used in
       typing. *)
    let escapable_string_literals = ordinary_string

    let comments =
      [ Nested_multiline ("/*", "*/")
      ]
  end
end

module Coq = struct
  module Info = struct
    let name = "Coq"
    let extensions = [".v"]
  end

  module Syntax = struct
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @
      [ "{|", "|}"
      ; "Proof", "Qed"
      ; "Proof", "Defined"
      ; "match", "end"
      ]

    (* Excludes ' as escapable string literal, since these can be used in
       typing. *)
    let escapable_string_literals = ordinary_string

    let comments =
      [ Nested_multiline ("(*", "*)")
      ]
  end
end

module Fsharp = struct
  module Info = struct
    let name = "F#"
    let extensions = [".fsx"]
  end

  module Syntax = OCaml.Syntax
end

(** Follow Free Pascal that allows nested comments, although Rosetta takes the opposite view. *)
module Pascal = struct
  module Info = struct
    let name = "Pascal"
    let extensions = [".pas"]
  end

  module Syntax = struct
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("(*", "*)")
      ; Nested_multiline ("{", "}")
      ; Until_newline "//"
      ]
  end
end

module Julia = struct
  module Info = struct
    let name = "Julia"
    let extensions = [".jl"]
  end

  module Syntax = struct
    include Generic.Syntax

    let user_defined_delimiters =
      Generic.Syntax.user_defined_delimiters
      @
      [ "if", "end"
      ; "for", "end"
      ; "while", "end"
      ; "try", "end"
      ; "struct", "end"
      ; "begin", "end"
      ; "let", "end"
      ]

    let comments =
      [ Nested_multiline ("#=", "=#")
      ; Until_newline "#"
      ]
  end
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
    include Generic.Syntax

    let comments =
      [ Until_newline "!"
      ]
  end
end

module Haskell = struct
  module Info = struct
    let name = "Haskell"
    let extensions = [".hs"]
  end

  module Syntax = struct
    include Generic.Syntax

    let raw_string_literals =
      [ ({|"""|}, {|"""|})
      ]

    (* Excludes ' as escapable string literal, since these can be used in
       identifiers. *)
    let escapable_string_literals = ordinary_string

    let comments =
      [ Multiline ("{-", "-}")
      ; Until_newline "--"
      ]
  end
end

module Elm = struct
  module Info = struct
    let name = "Elm"
    let extensions = [".elm"]
  end

  module Syntax = Haskell.Syntax
end

module Zig = struct
  module Info = struct
    let name = "Zig"
    let extensions =
      [ ".zig"
      ]
  end

  module Syntax = struct
    include Generic.Syntax

    let comments =
      [ Until_newline "//"
      ]

    (* Multiline strings with \\ are awkward to support. Maybe later. *)
    let escapable_string_literals = ordinary_string
  end
end

(** For testing *)
module C_nested_comments = struct
  module Info = struct
    let name = "C_with_nested_comments"
    let extensions = []
  end

  module Syntax = struct
    include Generic.Syntax

    let comments =
      [ Nested_multiline ("/*", "*/")
      ]
  end
end

let all: (module Types.Language.S) list =
  [ (module Bash)
  ; (module C)
  ; (module Csharp)
  ; (module CSS)
  ; (module Dart)
  ; (module Dyck)
  ; (module Clojure)
  ; (module Coq)
  ; (module Elm)
  ; (module Erlang)
  ; (module Elixir)
  ; (module Fortran)
  ; (module Fsharp)
  ; (module Go)
  ; (module Html)
  ; (module Haskell)
  ; (module Java)
  ; (module Javascript)
  ; (module Jsx)
  ; (module JSON)
  ; (module JSONC)
  ; (module GraphQL)
  ; (module Dhall)
  ; (module Julia)
  ; (module Kotlin)
  ; (module Latex)
  ; (module Lisp)
  ; (module Move)
  ; (module Nim)
  ; (module OCaml)
  ; (module Paren)
  ; (module Pascal)
  ; (module Php)
  ; (module Python)
  ; (module Reason)
  ; (module Ruby)
  ; (module Rust)
  ; (module Scala)
  ; (module Solidity)
  ; (module SQL)
  ; (module Swift)
  ; (module Text)
  ; (module Typescript)
  ; (module Tsx)
  ; (module Xml)
  ; (module Zig)
  ; (module Generic)
  ]

let select_with_extension extension : (module Types.Language.S) option =
  List.find all ~f:(fun (module M) -> List.exists M.Info.extensions ~f:(String.(=) extension))
