open Core

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

  type t = {
    user_defined_delimiters : (string * string) list;
    escapable_string_literals : escapable_string_literals option; [@default None]
    raw_string_literals : (string * string) list;
    comments : comment_kind list;
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

  type t =
    { sort : sort
    ; identifier : string
    ; dimension : dimension
    ; optional : bool
    }

  let sorts () =
    [ Everything
    ; Expression
    ; Alphanum
    ; Non_space
    ; Line
    ; Blank
    ]
end

type hole = Hole.t

module Omega = struct
  type omega_match_production =
    { offset : int
    ; identifier : string
    ; text : string
    }
  [@@deriving yojson]

  type production =
    | Unit
    | String of string
    | Template_string of string
    | Hole of hole
    | Match of omega_match_production
end

type production =
  | Unit
  | String of string
  | Hole of hole

module Matcher = struct
  module type S = sig
    include Info.S

    val first
      :  ?configuration:Configuration.t
      -> ?shift:int
      -> string
      -> string
      -> Match.t Or_error.t

    val set_rewrite_template : string -> unit

    val all
      :  ?configuration:Configuration.t
      -> template:string
      -> source:string
      -> Match.t list
  end
end

module Match_engine = struct
  module type S = sig
    module Text : Matcher.S
    module Paren : Matcher.S
    module Dyck : Matcher.S
    module Json : Matcher.S
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
    module Typescript : Matcher.S
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
    module C_nested_comments : Matcher.S

    val create : Syntax.t -> (module Matcher.S)
    val select_with_extension : string -> (module Matcher.S) option
    val all : (module Matcher.S) list
  end
end
