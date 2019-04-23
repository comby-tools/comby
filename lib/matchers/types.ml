open Core

type comment_kind =
  | Multiline of string * string
  | Until_newline of string

module Syntax = struct
  module type S = sig
    val user_defined_delimiters : (string * string) list
    val escapable_string_literals : string list
    val escape_char : char
    val raw_string_literals : (string * string) list
    val comment_parser : comment_kind list
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
  | Lazy of (id * dimension)
  | Single of (id * including * until * dimension)

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
