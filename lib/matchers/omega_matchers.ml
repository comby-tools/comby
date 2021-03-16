open Core
open Languages

module Matcher = Omega

module Text = Matcher.Make (Text)
module Paren = Matcher.Make (Paren)
module Dyck = Matcher.Make (Dyck)
module JSON = Matcher.Make (JSON)
module JSONC = Matcher.Make (JSONC)
module GraphQL = Matcher.Make (GraphQL)
module Dhall = Matcher.Make (Dhall)
module Latex = Matcher.Make (Latex)
module Assembly = Matcher.Make (Assembly)
module Clojure = Matcher.Make (Clojure)
module Lisp = Matcher.Make (Lisp)
module Generic = Matcher.Make (Generic)
module Bash = Matcher.Make (Bash)
module Ruby = Matcher.Make (Ruby)
module Elixir = Matcher.Make (Elixir)
module Python = Matcher.Make (Python)
module Html = Matcher.Make (Html)
module Xml = Matcher.Make (Xml)
module SQL = Matcher.Make (SQL)
module Erlang = Matcher.Make (Erlang)
module C = Matcher.Make (C)
module Csharp = Matcher.Make (Csharp)
module Java = Matcher.Make (Java)
module CSS = Matcher.Make (CSS)
module Kotlin = Matcher.Make (Kotlin)
module Scala = Matcher.Make (Scala)
module Nim = Matcher.Make (Nim)
module Dart = Matcher.Make (Dart)
module Php = Matcher.Make (Php)
module Go = Matcher.Make (Go)
module Javascript = Matcher.Make (Javascript)
module Jsx = Matcher.Make (Jsx)
module Typescript = Matcher.Make (Typescript)
module Tsx = Matcher.Make (Tsx)
module Swift = Matcher.Make (Swift)
module Rust = Matcher.Make (Rust)
module OCaml = Matcher.Make (OCaml)
module Reason = Matcher.Make (Reason)
module Fsharp = Matcher.Make (Fsharp)
module Pascal = Matcher.Make (Pascal)
module Julia = Matcher.Make (Julia)
module Fortran = Matcher.Make (Fortran)
module Haskell = Matcher.Make (Haskell)
module Elm = Matcher.Make (Elm)
module Zig = Matcher.Make (Zig)
module Coq  = Matcher.Make (Coq)
module Move = Matcher.Make (Move)
module Solidity = Matcher.Make (Solidity)
module C_nested_comments = Matcher.Make (C_nested_comments)

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

let select_with_extension ?(metasyntax) extension : (module Types.Matcher.S) option =
  let _ = metasyntax in
  List.find all ~f:(fun (module M) -> List.exists M.extensions ~f:(String.(=) extension))

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
  let _ = metasyntax in
  (module Omega.Make (User_language) : Types.Matcher.S)
