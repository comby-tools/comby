open Core_kernel

module Language = struct
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
  type alias =
    { pattern : string
    ; match_template : string
    ; rule : string option
    }
  [@@deriving yojson]

  type hole_definition =
    | Delimited of string option * string option
    | Reserved_identifiers of string list
  [@@deriving yojson]

  type hole_syntax =
    | Hole of Hole.sort * hole_definition
    | Regex of string * char * string
  [@@deriving yojson]

  type t =
    { syntax : hole_syntax list
    ; identifier : string
    ; aliases : alias list
    }
  [@@deriving yojson]

  module type S = sig
    val syntax : hole_syntax list
    val identifier : string
    val aliases : alias list
  end
end

type production =
  | Unit
  | String of string
  | Hole of hole

module Template = struct
  type kind =
    | Value
    | Length
    | Lines
    | OffsetStart
    | OffsetEnd
    | LineStart
    | LineEnd
    | ColumnStart
    | ColumnEnd
    | LsifHover
    | FileName
    | FilePath
    | FileDirectory
    | Lowercase
    | Uppercase
    | Capitalize
    | Uncapitalize
    | UpperCamelCase
    | LowerCamelCase
    | UpperSnakeCase
    | LowerSnakeCase
  [@@deriving sexp]

  type syntax =
    { variable: string (* E.g., x *)
    ; pattern: string (* E.g., the entire :[x] part *)
    ; offset : int
    ; kind : kind (* The kind of hole, to inform substitution *)
    }
  [@@deriving sexp]

  type atom =
    | Hole of syntax
    | Constant of string
  [@@deriving sexp]

  type t = atom list
  [@@deriving sexp]
end

module Ast = struct
  type atom =
    | Template of Template.t
    | String of string
  [@@deriving sexp]

  type antecedent = atom
  [@@deriving sexp]

  type expression =
    | True
    | False
    | Option of string
    | Equal of atom * atom
    | Not_equal of atom * atom
    | Match of atom * (antecedent * consequent) list
    | Rewrite of atom * (antecedent * atom)
  and consequent = expression list
  [@@deriving sexp]
end

module Rule = struct
  type t = Ast.expression list
  [@@deriving sexp]
end

module Matcher = struct
  module type S = sig
    val all
      :  ?configuration:Configuration.t
      -> ?filepath:string
      -> ?rule:Rule.t
      -> template:string
      -> source:string
      -> unit
      -> Match.t list

    val first
      :  ?configuration:Configuration.t
      -> ?shift:int
      -> ?filepath:string
      -> string
      -> string
      -> Match.t Or_error.t

    include Language.Info.S

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
    module HCL : Matcher.S
    module Elm : Matcher.S
    module Zig: Matcher.S
    module Coq: Matcher.S
    module Move: Matcher.S
    module Solidity: Matcher.S
    module C_nested_comments : Matcher.S

    val all : (module Matcher.S) list
    val select_with_extension : ?metasyntax:Metasyntax.t -> string -> (module Matcher.S) option
    val create : ?metasyntax:Metasyntax.t -> Language.Syntax.t -> (module Matcher.S)
  end
end
