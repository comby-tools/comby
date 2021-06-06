open Core_kernel

open Languages

module Make (Make : Types.Language.S -> Types.Metasyntax.S -> Types.External.S -> Types.Matcher.S) : Types.Engine.S = struct
  module Make = Make

  let create
      ?(metasyntax = Metasyntax.default_metasyntax)
      ?(external_handler = External.default_external)
      Types.Language.Syntax.
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
    let module External = struct let handler = external_handler end in
    (module Make (User_language) (Metasyntax) (External) : Types.Matcher.S)

  module Text = Make (Text) (Metasyntax.Default) (External.Default)
  module Paren = Make (Paren) (Metasyntax.Default) (External.Default)
  module Dyck = Make (Dyck) (Metasyntax.Default) (External.Default)
  module JSON = Make (JSON) (Metasyntax.Default) (External.Default)
  module JSONC = Make (JSONC) (Metasyntax.Default) (External.Default)
  module GraphQL = Make (GraphQL) (Metasyntax.Default) (External.Default)
  module Dhall = Make (Dhall) (Metasyntax.Default) (External.Default)
  module Latex = Make (Latex) (Metasyntax.Default) (External.Default)
  module Assembly = Make (Assembly) (Metasyntax.Default) (External.Default)
  module Clojure = Make (Clojure) (Metasyntax.Default) (External.Default)
  module Lisp = Make (Lisp) (Metasyntax.Default) (External.Default)
  module Generic = Make (Generic) (Metasyntax.Default) (External.Default)
  module Bash = Make (Bash) (Metasyntax.Default) (External.Default)
  module Ruby = Make (Ruby) (Metasyntax.Default) (External.Default)
  module Elixir = Make (Elixir) (Metasyntax.Default) (External.Default)
  module Python = Make (Python) (Metasyntax.Default) (External.Default)
  module Html = Make (Html) (Metasyntax.Default) (External.Default)
  module Xml = Make (Xml) (Metasyntax.Default) (External.Default)
  module SQL = Make (SQL) (Metasyntax.Default) (External.Default)
  module Erlang = Make (Erlang) (Metasyntax.Default) (External.Default)
  module C = Make (C) (Metasyntax.Default) (External.Default)
  module Csharp = Make (Csharp) (Metasyntax.Default) (External.Default)
  module Java = Make (Java) (Metasyntax.Default) (External.Default)
  module CSS = Make (CSS) (Metasyntax.Default) (External.Default)
  module Kotlin = Make (Kotlin) (Metasyntax.Default) (External.Default)
  module Scala = Make (Scala) (Metasyntax.Default) (External.Default)
  module Nim = Make (Nim) (Metasyntax.Default) (External.Default)
  module Matlab = Make (Matlab) (Metasyntax.Default) (External.Default)
  module Dart = Make (Dart) (Metasyntax.Default) (External.Default)
  module Php = Make (Php) (Metasyntax.Default) (External.Default)
  module Go = Make (Go) (Metasyntax.Default) (External.Default)
  module Javascript = Make (Javascript) (Metasyntax.Default) (External.Default)
  module Jsx = Make (Jsx) (Metasyntax.Default) (External.Default)
  module Typescript = Make (Typescript) (Metasyntax.Default) (External.Default)
  module Tsx = Make (Tsx) (Metasyntax.Default) (External.Default)
  module Swift = Make (Swift) (Metasyntax.Default) (External.Default)
  module Rust = Make (Rust) (Metasyntax.Default) (External.Default)
  module R = Make (R) (Metasyntax.Default) (External.Default)
  module OCaml = Make (OCaml) (Metasyntax.Default) (External.Default)
  module Reason = Make (Reason) (Metasyntax.Default) (External.Default)
  module Fsharp = Make (Fsharp) (Metasyntax.Default) (External.Default)
  module Pascal = Make (Pascal) (Metasyntax.Default) (External.Default)
  module Julia = Make (Julia) (Metasyntax.Default) (External.Default)
  module Fortran = Make (Fortran) (Metasyntax.Default) (External.Default)
  module Haskell = Make (Haskell) (Metasyntax.Default) (External.Default)
  module HCL = Make (HCL) (Metasyntax.Default) (External.Default)
  module Elm = Make (Elm) (Metasyntax.Default) (External.Default)
  module Zig = Make (Zig) (Metasyntax.Default) (External.Default)
  module Coq  = Make (Coq) (Metasyntax.Default) (External.Default)
  module Move = Make (Move) (Metasyntax.Default) (External.Default)
  module Solidity = Make (Solidity) (Metasyntax.Default) (External.Default)
  module C_nested_comments = Make (C_nested_comments) (Metasyntax.Default) (External.Default)

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
    ; (module HCL)
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
    ; (module Matlab)
    ; (module OCaml)
    ; (module Paren)
    ; (module Pascal)
    ; (module Php)
    ; (module Python)
    ; (module Reason)
    ; (module R)
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

  let select_with_extension
      ?(metasyntax = Metasyntax.default_metasyntax)
      ?(external_handler = External.default_external)
      extension
    : (module Types.Matcher.S) option =
    let open Option in
    Languages.select_with_extension extension >>| fun (module Language : Types.Language.S) ->
    let (module Metasyntax) = Metasyntax.(create metasyntax) in
    let module External = struct let handler = external_handler end in
    (module (Make (Language) (Metasyntax) (External)) : Types.Matcher.S)
end
