module Matchers : sig
  module Configuration : module type of Matchers.Configuration
  module Syntax : module type of Matchers.Syntax

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

  module type Matcher = Matchers.Matcher

  module Alpha : sig
    module Text : Matcher
    module Paren : Matcher
    module Dyck : Matcher
    module JSON : Matcher
    module JSONC : Matcher
    module GraphQL : Matcher
    module Dhall : Matcher
    module Latex : Matcher
    module Assembly : Matcher
    module Clojure : Matcher
    module Lisp : Matcher
    module Generic : Matcher
    module Bash : Matcher
    module Ruby : Matcher
    module Elixir : Matcher
    module Python : Matcher
    module Html : Matcher
    module Xml : Matcher
    module SQL : Matcher
    module Erlang : Matcher
    module C : Matcher
    module Csharp : Matcher
    module Java : Matcher
    module CSS : Matcher
    module Kotlin : Matcher
    module Scala : Matcher
    module Nim : Matcher
    module Dart : Matcher
    module Php : Matcher
    module Go : Matcher
    module Javascript : Matcher
    module Jsx : Matcher
    module Typescript : Matcher
    module Tsx : Matcher
    module Swift : Matcher
    module Rust : Matcher
    module OCaml : Matcher
    module Reason : Matcher
    module Fsharp : Matcher
    module Pascal : Matcher
    module Julia : Matcher
    module Fortran : Matcher
    module Haskell : Matcher
    module Elm : Matcher
    module Zig : Matcher
    module Coq : Matcher
    module Move : Matcher
    module Solidity : Matcher
    module C_nested_comments : Matcher

    val all : (module Matcher) list

    val create : ?metasyntax:Metasyntax.t -> Syntax.t -> (module Matcher)

    val select_with_extension : ?metasyntax:Metasyntax.t -> string -> (module Matcher) option
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
