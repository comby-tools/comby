open Types

module Text : Language.S
module Paren : Language.S
module Dyck : Language.S
module JSON : Language.S
module JSONC : Language.S
module GraphQL : Language.S
module Dhall : Language.S
module Latex : Language.S
module Assembly : Language.S
module Clojure : Language.S
module Lisp : Language.S
module Generic : Language.S
module Bash : Language.S
module Ruby : Language.S
module Elixir : Language.S
module Python : Language.S
module Html : Language.S
module Xml : Language.S
module SQL : Language.S
module Erlang : Language.S
module C : Language.S
module Csharp : Language.S
module Java : Language.S
module CSS : Language.S
module Kotlin : Language.S
module Scala : Language.S
module Nim : Language.S
module Dart : Language.S
module Php : Language.S
module Go : Language.S
module Javascript : Language.S
module Jsx : Language.S
module Typescript : Language.S
module Tsx : Language.S
module Swift : Language.S
module Rust : Language.S
module OCaml : Language.S
module Reason : Language.S
module Fsharp : Language.S
module Pascal : Language.S
module Julia : Language.S
module Fortran : Language.S
module Haskell : Language.S
module Elm : Language.S
module Zig : Language.S
module Coq : Language.S
module Move : Language.S
module Solidity : Language.S
module C_nested_comments : Language.S

val all : (module Language.S) list

val select_with_extension : string -> (module Language.S) option
