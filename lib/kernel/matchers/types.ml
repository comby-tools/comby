open Core_kernel

module Syntax = struct
  type escapable_string_literals =
    { delimiters : string list
    ; escape_character: char
    }
  [@@deriving yojson]

  type comment_kind =
    | Multiline of string * string
    | Nested_multiline of string * string
    | Until_newline of string
  [@@deriving yojson]

  type t =
    { user_defined_delimiters : (string * string) list
    ; escapable_string_literals : escapable_string_literals option [@default None]
    ; raw_string_literals : (string * string) list
    ; comments : comment_kind list
    }
  [@@deriving yojson]

  module type S = sig
    val user_defined_delimiters : (string * string) list
    val escapable_string_literals : escapable_string_literals option
    val raw_string_literals : (string * string) list
    val comments : comment_kind list
  end
end

module Info = struct
  module type S = sig
    val name : string
    val extensions : string list
  end
end

module Language = struct
  module type S = sig
    module Info : Info.S
    module Syntax : Syntax.S
  end
end


type dimension =
  | Code
  | Escapable_string_literal
  | Raw_string_literal
  | Comment

type id = string
type including = char list
type until = char option

module Hole = struct

  type sort =
    | Everything
    | Expression
    | Alphanum
    | Non_space
    | Line
    | Blank
    | Regex
  [@@deriving yojson]

  type t =
    { sort : sort
    ; identifier : string
    ; dimension : dimension
    ; optional : bool
    ; at_depth : int option
    }

  let sorts () =
    [ Everything
    ; Expression
    ; Alphanum
    ; Non_space
    ; Line
    ; Blank
    ; Regex
    ]
end

type hole = Hole.t

module Metasyntax = struct

  type hole_definition =
    | Delimited of string option * string option
  [@@deriving yojson]

  type hole_syntax =
    | Hole of Hole.sort * hole_definition
    | Regex of string * char * string
  [@@deriving yojson]

  type t =
    { syntax : hole_syntax list
    ; identifier : string
    }
  [@@deriving yojson]

  module type S = sig
    val syntax : hole_syntax list
    val identifier : string
  end
end

type production =
  | Unit
  | String of string
  | Hole of hole

module Matcher = struct
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

    include Info.S

    val set_rewrite_template : string -> unit

  end
end

module Engine = struct
  module type S = sig
    module Make : Language.S -> Metasyntax.S -> Matcher.S

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
    module Zig: Matcher.S
    module Coq: Matcher.S
    module Move: Matcher.S
    module Solidity: Matcher.S
    module C_nested_comments : Matcher.S

    val all : (module Matcher.S) list
    val select_with_extension : ?metasyntax:Metasyntax.t -> string -> (module Matcher.S) option
    val create : ?metasyntax:Metasyntax.t -> Syntax.t -> (module Matcher.S)
  end
end
