open Core

module Matchers : sig
  module Configuration : sig
    type t

    type match_kind =
      | Exact
      | Fuzzy

    val create
      :  ?disable_substring_matching:bool
      -> ?match_kind:match_kind
      -> ?significant_whitespace:bool
      -> ?match_newline_toplevel:bool
      -> unit
      -> t
  end

  module Syntax : sig
    type escapable_string_literals =
      { delimiters : string list
      ; escape_character: char
      }

    type comment_kind =
      | Multiline of string * string
      | Nested_multiline of string * string
      | Until_newline of string

    type t =
      { user_defined_delimiters : (string * string) list
      ; escapable_string_literals : escapable_string_literals option [@default None]
      ; raw_string_literals : (string * string) list
      ; comments : comment_kind list
      }

    val to_yojson : t -> Yojson.Safe.json
    val of_yojson : Yojson.Safe.json -> (t, string) Result.t

    module type S = sig
      val user_defined_delimiters : (string * string) list
      val escapable_string_literals : escapable_string_literals option
      val raw_string_literals : (string * string) list
      val comments : comment_kind list
    end
  end

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

  module Matcher : sig
    module type S = sig
      val all
        :  ?configuration:Configuration.t
        -> ?nested: bool
        -> template:string
        -> source:string
        -> unit
        -> Match.t list

      val first
        :  ?configuration:Configuration.t
        -> ?shift:int
        -> string
        -> string
        -> Match.t Or_error.t

      val name : string

      val extensions : string list

      val set_rewrite_template : string -> unit
    end
  end

  module Alpha : sig
    val select_with_extension : ?metasyntax:Metasyntax.t -> string -> (module Matcher.S) option

    val create : ?metasyntax:Metasyntax.t -> Syntax.t -> (module Matcher.S)

    val all : (module Matcher.S) list

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
    module Dart : Matcher.S
    module Php : Matcher.S
    module Go : Matcher.S
    module Javascript : Matcher.S
    module Jsx : Matcher.S
    module Typescript : Matcher.S
    module Tsx : Matcher.S
    module Swift : Matcher.S
    module Rust : Matcher.S
    module OCaml : Matcher.S
    module Reason : Matcher.S
    module Fsharp : Matcher.S
    module Pascal : Matcher.S
    module Julia : Matcher.S
    module Fortran : Matcher.S
    module Haskell : Matcher.S
    module Elm : Matcher.S
    module Zig : Matcher.S
    module Coq : Matcher.S
    module Move : Matcher.S
    module Solidity : Matcher.S
    module C_nested_comments : Matcher.S
  end

(*
  module Omega = Omega
  module Languages = Languages
*)
end

module Specification : sig
  type t

  val create
    :  ?rewrite_template:string
    -> ?rule:Language.Rule.t
    -> match_template:string
    -> unit
    -> t
end

module Pipeline : sig

  val with_timeout : int -> Configuration.Command_input.single_source -> f:(unit -> 'a list) -> 'a list

  val timed_run
    :  (module Matchers.Matcher.S)
    -> ?fast_offset_conversion:bool
    -> ?omega:bool
    -> ?substitute_in_place:bool
    -> configuration:Matchers.Configuration.t
    -> source:string
    -> specification:Specification.t
    -> unit
    -> Match.t list
end

module Language = Language
module Match = Match
module Replacement = Replacement
module Rewriter = Rewriter
module Statistics = Statistics
module Configuration = Configuration
