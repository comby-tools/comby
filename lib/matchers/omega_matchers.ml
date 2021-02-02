open Core
open Languages

module Matcher = Omega

module Text = Matcher.Make (Text.Syntax) (Text.Info)
module Paren = Matcher.Make (Paren.Syntax) (Paren.Info)
module Dyck = Matcher.Make (Dyck.Syntax) (Dyck.Info)
module JSON = Matcher.Make (JSON.Syntax) (JSON.Info)
module JSONC = Matcher.Make (JSONC.Syntax) (JSONC.Info)
module GraphQL = Matcher.Make (GraphQL.Syntax) (GraphQL.Info)
module Dhall = Matcher.Make (Dhall.Syntax) (Dhall.Info)
module Latex = Matcher.Make (Latex.Syntax) (Latex.Info)
module Assembly = Matcher.Make (Assembly.Syntax) (Assembly.Info)
module Clojure = Matcher.Make (Clojure.Syntax) (Clojure.Info)
module Lisp = Matcher.Make (Lisp.Syntax) (Lisp.Info)
module Generic = Matcher.Make (Generic.Syntax) (Generic.Info)
module Bash = Matcher.Make (Bash.Syntax) (Bash.Info)
module Ruby = Matcher.Make (Ruby.Syntax) (Ruby.Info)
module Elixir = Matcher.Make (Elixir.Syntax) (Elixir.Info)
module Python = Matcher.Make (Python.Syntax) (Python.Info)
module Html = Matcher.Make (Html.Syntax) (Html.Info)
module Xml = Matcher.Make (Xml.Syntax) (Xml.Info)
module SQL = Matcher.Make (SQL.Syntax) (SQL.Info)
module Erlang = Matcher.Make (Erlang.Syntax) (Erlang.Info)
module C = Matcher.Make (C.Syntax) (C.Info)
module Csharp = Matcher.Make (Csharp.Syntax) (Csharp.Info)
module Java = Matcher.Make (Java.Syntax) (Java.Info)
module CSS = Matcher.Make (CSS.Syntax) (CSS.Info)
module Kotlin = Matcher.Make (Kotlin.Syntax) (Kotlin.Info)
module Scala = Matcher.Make (Scala.Syntax) (Scala.Info)
module Nim = Matcher.Make (Nim.Syntax) (Nim.Info)
module Dart = Matcher.Make (Dart.Syntax) (Dart.Info)
module Php = Matcher.Make (Php.Syntax) (Php.Info)
module Go = Matcher.Make (Go.Syntax) (Go.Info)
module Javascript = Matcher.Make (Javascript.Syntax) (Javascript.Info)
module Jsx = Matcher.Make (Jsx.Syntax) (Jsx.Info)
module Typescript = Matcher.Make (Typescript.Syntax) (Typescript.Info)
module Tsx = Matcher.Make (Tsx.Syntax) (Tsx.Info)
module Swift = Matcher.Make (Swift.Syntax) (Swift.Info)
module Rust = Matcher.Make (Rust.Syntax) (Rust.Info)
module OCaml = Matcher.Make (OCaml.Syntax) (OCaml.Info)
module Reason = Matcher.Make (Reason.Syntax) (Reason.Info)
module Fsharp = Matcher.Make (Fsharp.Syntax) (Fsharp.Info)
module Pascal = Matcher.Make (Pascal.Syntax) (Pascal.Info)
module Julia = Matcher.Make (Julia.Syntax) (Julia.Info)
module Fortran = Matcher.Make (Fortran.Syntax) (Fortran.Info)
module Haskell = Matcher.Make (Haskell.Syntax) (Haskell.Info)
module Elm = Matcher.Make (Elm.Syntax) (Elm.Info)
module Zig = Matcher.Make (Zig.Syntax) (Zig.Info)
module Coq  = Matcher.Make (Coq.Syntax) (Coq.Info)
module Move = Matcher.Make (Move.Syntax) (Move.Info)
module Solidity = Matcher.Make (Solidity.Syntax) (Solidity.Info)
module C_nested_comments = Matcher.Make (C_nested_comments.Syntax) (C_nested_comments.Info)

let all : (module Types.Matcher.S) list =
  [ (module Assembly)
  ; (module Bash)
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

let select_with_extension extension : (module Types.Matcher.S) option =
  List.find all ~f:(fun (module M) -> List.exists M.extensions ~f:(String.(=) extension))

let create
    Types.Syntax.
      { user_defined_delimiters
      ; escapable_string_literals
      ; raw_string_literals
      ; comments
      } =
  let module Info = struct
    let name = "User_defined_language"
    let extensions = []
  end
  in
  let module User_language = struct
    let user_defined_delimiters = user_defined_delimiters
    let escapable_string_literals = escapable_string_literals
    let raw_string_literals = raw_string_literals
    let comments = comments
  end
  in
  (module Omega.Make (User_language) (Info) : Types.Matcher.S)
