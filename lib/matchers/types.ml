open Core

type comment_kind =
  | Multiline of string * string
  | Nested_multiline of string * string
  | Until_newline of string
[@@deriving yojson]

module Syntax = struct

  type escapable_string_literals =
    { delimiters : string list
    ; escape_character: char
    }
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

type dimension =
  | Code
  | Escapable_string_literal
  | Raw_string_literal
  | Comment

type id = string
type including = char list
type until = char option

type hole =
  | Everything of (id * dimension)
  | Alphanum of (id * dimension)
  | Non_space of (id * dimension)
  | Line of (id * dimension)
  | Blank of (id * dimension)

type production =
  | Unit
  | String of string
  | Hole of hole
  | Match of (int * string * string)

module Matcher = struct
  module type S = sig
    include Info.S

    val first
      :  ?configuration:Configuration.t
      -> ?shift:int
      -> string
      -> string
      -> Match.t Or_error.t

    val all
      :  ?configuration:Configuration.t
      -> template:string
      -> source:string
      -> Match.t list
  end
end
