open Core
open Languages

module Matcher = Alpha

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
  let (module Metasyntax) = Metasyntax.(create metasyntax) in
  (module Alpha.Make (User_language) (Metasyntax) : Types.Matcher.S)

module Text = Matcher.Make (Text) (Metasyntax.Default)
module Paren = Matcher.Make (Paren) (Metasyntax.Default)
module Dyck = Matcher.Make (Dyck) (Metasyntax.Default)
module JSON = Matcher.Make (JSON) (Metasyntax.Default)
module JSONC = Matcher.Make (JSONC) (Metasyntax.Default)
module GraphQL = Matcher.Make (GraphQL) (Metasyntax.Default)
module Dhall = Matcher.Make (Dhall) (Metasyntax.Default)
module Latex = Matcher.Make (Latex) (Metasyntax.Default)
module Assembly = Matcher.Make (Assembly) (Metasyntax.Default)
module Clojure = Matcher.Make (Clojure) (Metasyntax.Default)
module Lisp = Matcher.Make (Lisp) (Metasyntax.Default)
module Generic = Matcher.Make (Generic) (Metasyntax.Default)
module Bash = Matcher.Make (Bash) (Metasyntax.Default)
module Ruby = Matcher.Make (Ruby) (Metasyntax.Default)
module Elixir = Matcher.Make (Elixir) (Metasyntax.Default)
module Python = Matcher.Make (Python) (Metasyntax.Default)
module Html = Matcher.Make (Html) (Metasyntax.Default)
module Xml = Matcher.Make (Xml) (Metasyntax.Default)
module SQL = Matcher.Make (SQL) (Metasyntax.Default)
module Erlang = Matcher.Make (Erlang) (Metasyntax.Default)
module C = Matcher.Make (C) (Metasyntax.Default)
module Csharp = Matcher.Make (Csharp) (Metasyntax.Default)
module Java = Matcher.Make (Java) (Metasyntax.Default)
module CSS = Matcher.Make (CSS) (Metasyntax.Default)
module Kotlin = Matcher.Make (Kotlin) (Metasyntax.Default)
module Scala = Matcher.Make (Scala) (Metasyntax.Default)
module Nim = Matcher.Make (Nim) (Metasyntax.Default)
module Dart = Matcher.Make (Dart) (Metasyntax.Default)
module Php = Matcher.Make (Php) (Metasyntax.Default)
module Go = Matcher.Make (Go) (Metasyntax.Default)
module Javascript = Matcher.Make (Javascript) (Metasyntax.Default)
module Jsx = Matcher.Make (Jsx) (Metasyntax.Default)
module Typescript = Matcher.Make (Typescript) (Metasyntax.Default)
module Tsx = Matcher.Make (Tsx) (Metasyntax.Default)
module Swift = Matcher.Make (Swift) (Metasyntax.Default)
module Rust = Matcher.Make (Rust) (Metasyntax.Default)
module OCaml = Matcher.Make (OCaml) (Metasyntax.Default)
module Reason = Matcher.Make (Reason) (Metasyntax.Default)
module Fsharp = Matcher.Make (Fsharp) (Metasyntax.Default)
module Pascal = Matcher.Make (Pascal) (Metasyntax.Default)
module Julia = Matcher.Make (Julia) (Metasyntax.Default)
module Fortran = Matcher.Make (Fortran) (Metasyntax.Default)
module Haskell = Matcher.Make (Haskell) (Metasyntax.Default)
module Elm = Matcher.Make (Elm) (Metasyntax.Default)
module Zig = Matcher.Make (Zig) (Metasyntax.Default)
module Coq  = Matcher.Make (Coq) (Metasyntax.Default)
module Move = Matcher.Make (Move) (Metasyntax.Default)
module Solidity = Matcher.Make (Solidity) (Metasyntax.Default)
module C_nested_comments = Matcher.Make (C_nested_comments) (Metasyntax.Default)

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
  List.find_map Languages.all ~f:(fun (module M) ->
      if List.exists M.Info.extensions ~f:(String.(=) extension) then
        let (module Metasyntax) = Metasyntax.(create metasyntax) in
        Some (module (Matcher.Make (M) (Metasyntax)) : Types.Matcher.S)
      else
        None)
