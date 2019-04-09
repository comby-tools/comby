open Core

module Syntax = struct
  module type S = sig
    val user_defined_delimiters : (string * string) list
    val escapable_string_literals : string list
    val escape_char : char
    val raw_string_literals : (string * string) list
    val comment_parser : (string, _) MParser.t
  end
end

type dimension =
  | Code
  | Escapable_string_literal
  | Raw_string_literal
  | Comment

type hole =
  | Lazy of (string * dimension)
  | Single of (string * dimension)

type production =
  | Unit
  | String of string
  | Hole of hole
  | Match of (int * string * string)

module Matcher = struct
  module type S = sig
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
