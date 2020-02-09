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
    | Alphanum
    | Non_space
    | Line
    | Blank
    | Regex

  type t =
    { sort : sort
    ; identifier : string
    ; dimension : dimension
    ; optional : bool
    }

  let sorts () =
    [ Everything
    ; Alphanum
    ; Non_space
    ; Line
    ; Blank
    ; Regex
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
