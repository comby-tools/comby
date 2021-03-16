module Matchers : sig
  module Configuration : module type of Matchers.Configuration
  module Syntax : module type of Matchers.Syntax

  (*module Matcher : Matchers.Matcher*)

  module Hole : sig
    type sort =
      | Everything
      | Expression
      | Alphanum
      | Non_space
      | Line
      | Blank
      | Regex
  end

  module Metasyntax : sig
    type hole_definition =
        Delimited of string option * string option

    type hole_syntax =
      | Hole of Hole.sort * hole_definition
      | Regex of string * char * string

    type t =
      { syntax : hole_syntax list
      ; identifier : char -> bool
      }

    module type S = sig
      val syntax : hole_syntax list
      val identifier : char -> bool
    end

    val default_metasyntax : t

    val create : t -> (module S)

    val default : (module S)

    module Default : S
  end

  module Alpha : sig
    module Text : Matchers.Matcher
    module Paren : Matchers.Matcher
    module Dyck : Matchers.Matcher
    module JSON : Matchers.Matcher
    module JSONC : Matchers.Matcher
    module GraphQL : Matchers.Matcher
    module Dhall : Matchers.Matcher
    module Latex : Matchers.Matcher
    module Assembly : Matchers.Matcher
    module Clojure : Matchers.Matcher
    module Lisp : Matchers.Matcher
    module Generic : Matchers.Matcher
    module Bash : Matchers.Matcher
    module Ruby : Matchers.Matcher
    module Elixir : Matchers.Matcher
    module Python : Matchers.Matcher
    module Html : Matchers.Matcher
    module Xml : Matchers.Matcher
    module SQL : Matchers.Matcher
    module Erlang : Matchers.Matcher
    module C : Matchers.Matcher
    module Csharp : Matchers.Matcher
    module Java : Matchers.Matcher
    module CSS : Matchers.Matcher
    module Kotlin : Matchers.Matcher
    module Scala : Matchers.Matcher
    module Nim : Matchers.Matcher
    module Dart : Matchers.Matcher
    module Php : Matchers.Matcher
    module Go : Matchers.Matcher
    module Javascript : Matchers.Matcher
    module Jsx : Matchers.Matcher
    module Typescript : Matchers.Matcher
    module Tsx : Matchers.Matcher
    module Swift : Matchers.Matcher
    module Rust : Matchers.Matcher
    module OCaml : Matchers.Matcher
    module Reason : Matchers.Matcher
    module Fsharp : Matchers.Matcher
    module Pascal : Matchers.Matcher
    module Julia : Matchers.Matcher
    module Fortran : Matchers.Matcher
    module Haskell : Matchers.Matcher
    module Elm : Matchers.Matcher
    module Zig : Matchers.Matcher
    module Coq : Matchers.Matcher
    module Move : Matchers.Matcher
    module Solidity : Matchers.Matcher
    module C_nested_comments : Matchers.Matcher

    val all : (module Matchers.Matcher) list

    val create : ?metasyntax:Metasyntax.t -> Syntax.t -> (module Matchers.Matcher)

    val select_with_extension : ?metasyntax:Metasyntax.t -> string -> (module Matchers.Matcher) option
  end

(*
  module Configuration = Configuration
  module Syntax = Syntax


  module Alpha : sig
    val select_with_extension : ?metasyntax:Metasyntax.t -> string -> (module Metasyntax.S) option
  end

  module Omega = Omega
  module Languages = Languages
*)
end

module Language = Language
module Match = Match
module Replacement = Replacement
module Rewriter = Rewriter
module Server_types = Server_types
module Statistics = Statistics
module Configuration = Configuration
