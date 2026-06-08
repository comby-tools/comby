open Types

module Make (_ : Language.S) (_ : Metasyntax.S) (_ : External.S) : Matcher.S
module Text : Matcher.S
module Paren : Matcher.S
module Dyck : Matcher.S
module JSON : Matcher.S
module JSONC : Matcher.S
module GraphQL : Matcher.S
module Dhall : Matcher.S
module Latex : Matcher.S
module Assembly : Matcher.S
module Clojure : Matcher.S
module Lisp : Matcher.S
module Generic : Matcher.S
module Bash : Matcher.S
module Ruby : Matcher.S
module Elixir : Matcher.S
module Python : Matcher.S
module Html : Matcher.S
module Xml : Matcher.S
module SQL : Matcher.S
module Erlang : Matcher.S
module C : Matcher.S
module Csharp : Matcher.S
module Java : Matcher.S
module CSS : Matcher.S
module Kotlin : Matcher.S
module Scala : Matcher.S
module Nim : Matcher.S
module Matlab : Matcher.S
module Dart : Matcher.S
module Php : Matcher.S
module Go : Matcher.S
module Javascript : Matcher.S
module Jsx : Matcher.S
module Typescript : Matcher.S
module Tsx : Matcher.S
module Swift : Matcher.S
module Rust : Matcher.S
module R : Matcher.S
module OCaml : Matcher.S
module Reason : Matcher.S
module Fsharp : Matcher.S
module Pascal : Matcher.S
module Julia : Matcher.S
module Fortran : Matcher.S
module Haskell : Matcher.S
module HCL : Matcher.S
module Elm : Matcher.S
module Zig : Matcher.S
module Coq : Matcher.S
module Move : Matcher.S
module Solidity : Matcher.S
module C_nested_comments : Matcher.S

val all : (module Matcher.S) list

val select_with_extension
  :  ?metasyntax:Metasyntax.t
  -> ?external_handler:External.t
  -> string
  -> (module Matcher.S) option

val create
  :  ?metasyntax:Metasyntax.t
  -> ?external_handler:External.t
  -> Language.Syntax.t
  -> (module Matcher.S)
