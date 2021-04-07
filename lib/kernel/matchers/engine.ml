open Core_kernel

open Languages

module Make (Make : Types.Language.S -> Types.Metasyntax.S -> Types.Matcher.S) : Types.Engine.S = struct
  module Make = Make

  let create
      ?(metasyntax = Metasyntax.default_metasyntax)
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
    let module Syntax = struct
      let user_defined_delimiters = user_defined_delimiters
      let escapable_string_literals = escapable_string_literals
      let raw_string_literals = raw_string_literals
      let comments = comments
    end
    in
    let module User_language = struct
      module Info = Info
      module Syntax = Syntax
    end
    in
    let (module Metasyntax : Metasyntax.S) = Metasyntax.(create metasyntax) in
    (module Make (User_language) (Metasyntax) : Types.Matcher.S)

  module Text = Make (Text) (Metasyntax.Default)
  module Paren = Make (Paren) (Metasyntax.Default)
  module Dyck = Make (Dyck) (Metasyntax.Default)
  module JSON = Make (JSON) (Metasyntax.Default)
  module JSONC = Make (JSONC) (Metasyntax.Default)
  module GraphQL = Make (GraphQL) (Metasyntax.Default)
  module Dhall = Make (Dhall) (Metasyntax.Default)
  module Latex = Make (Latex) (Metasyntax.Default)
  module Assembly = Make (Assembly) (Metasyntax.Default)
  module Clojure = Make (Clojure) (Metasyntax.Default)
  module Lisp = Make (Lisp) (Metasyntax.Default)
  module Generic = Make (Generic) (Metasyntax.Default)
  module Bash = Make (Bash) (Metasyntax.Default)
  module Ruby = Make (Ruby) (Metasyntax.Default)
  module Elixir = Make (Elixir) (Metasyntax.Default)
  module Python = Make (Python) (Metasyntax.Default)
  module Html = Make (Html) (Metasyntax.Default)
  module Xml = Make (Xml) (Metasyntax.Default)
  module SQL = Make (SQL) (Metasyntax.Default)
  module Erlang = Make (Erlang) (Metasyntax.Default)
  module C = Make (C) (Metasyntax.Default)
  module Csharp = Make (Csharp) (Metasyntax.Default)
  module Java = Make (Java) (Metasyntax.Default)
  module CSS = Make (CSS) (Metasyntax.Default)
  module Kotlin = Make (Kotlin) (Metasyntax.Default)
  module Scala = Make (Scala) (Metasyntax.Default)
  module Nim = Make (Nim) (Metasyntax.Default)
  module Dart = Make (Dart) (Metasyntax.Default)
  module Php = Make (Php) (Metasyntax.Default)
  module Go = Make (Go) (Metasyntax.Default)
  module Javascript = Make (Javascript) (Metasyntax.Default)
  module Jsx = Make (Jsx) (Metasyntax.Default)
  module Typescript = Make (Typescript) (Metasyntax.Default)
  module Tsx = Make (Tsx) (Metasyntax.Default)
  module Swift = Make (Swift) (Metasyntax.Default)
  module Rust = Make (Rust) (Metasyntax.Default)
  module OCaml = Make (OCaml) (Metasyntax.Default)
  module Reason = Make (Reason) (Metasyntax.Default)
  module Fsharp = Make (Fsharp) (Metasyntax.Default)
  module Pascal = Make (Pascal) (Metasyntax.Default)
  module Julia = Make (Julia) (Metasyntax.Default)
  module Fortran = Make (Fortran) (Metasyntax.Default)
  module Haskell = Make (Haskell) (Metasyntax.Default)
  module Elm = Make (Elm) (Metasyntax.Default)
  module Zig = Make (Zig) (Metasyntax.Default)
  module Coq  = Make (Coq) (Metasyntax.Default)
  module Move = Make (Move) (Metasyntax.Default)
  module Solidity = Make (Solidity) (Metasyntax.Default)
  module C_nested_comments = Make (C_nested_comments) (Metasyntax.Default)

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

  let select_with_extension ?(metasyntax = Metasyntax.default_metasyntax) extension : (module Types.Matcher.S) option =
    let open Option in
    Languages.select_with_extension extension >>| fun (module Language : Types.Language.S) ->
    let (module Metasyntax) = Metasyntax.(create metasyntax) in
    (module (Make (Language) (Metasyntax)) : Types.Matcher.S)
end
