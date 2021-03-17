open Core

module Match : sig
  module Location : sig
    type t =
      { offset : int
      ; line : int
      ; column : int
      }
    [@@deriving eq, sexp]

    val to_yojson : t -> Yojson.Safe.json
    val of_yojson : Yojson.Safe.json -> (t, string) Result.t

    val default : t
  end

  type location = Location.t
  [@@deriving eq, sexp]

  module Range : sig
    type t =
      { match_start : location [@key "start"]
      ; match_end : location [@key "end"]
      }
    [@@deriving eq, sexp]

    val to_yojson : t -> Yojson.Safe.json
    val of_yojson : Yojson.Safe.json -> (t, string) Result.t

    val default : t
  end

  type range = Range.t
  [@@deriving eq, sexp]

  module Environment : sig
    type t
    [@@deriving eq]

    val to_yojson : t -> Yojson.Safe.json
    val of_yojson : Yojson.Safe.json -> (t, string) Result.t

    val create : unit -> t

    val vars : t -> string list

    val add : ?range:range -> t -> string -> string -> t

    val lookup : t -> string -> string option

    val update : t -> string -> string -> t

    val lookup_range : t -> string -> range option

    val update_range : t -> string -> range -> t

    val furthest_match : t -> int

    val equal : t -> t -> bool

    val copy : t -> t

    val merge : t -> t -> t

    val to_string : t -> string

    val exists : t -> string -> bool
  end

  type environment = Environment.t

  type t =
    { range : range
    ; environment : environment
    ; matched : string
    }

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) Result.t

  val create : ?range:range -> unit -> t

  val convert_offset : fast:bool -> source:string -> t -> t

  val pp : Format.formatter -> string option * t list -> unit

  val pp_json_lines : Format.formatter -> string option * t list -> unit

  val pp_match_count : Format.formatter -> string option * t list -> unit

end

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

module Rewriter : sig
  module Rewrite : sig
    val all
      :  ?source:string
      -> ?sequential:bool
      -> rewrite_template:string
      -> Match.t list
      -> Replacement.result option
  end

  module Rewrite_template : sig
    (** if [sequential] is true, then substitute the pattern :[id()] starting at 1,
        and incrementing subsequent IDs. if [sequential] is false, then substitute
        the pattern :[id()] with a fresh hex string based on the last 48-bit part of
        a UUID v3 identifier *)
    val substitute_fresh : ?sequential:bool -> string -> string

    (** substitute returns the result and variables substituted for *)
    val substitute : ?sequential:bool -> string -> Match.Environment.t -> (string * string list)

    val of_match_context : Match.t -> source:string -> (string * string)

    val get_offsets_for_holes : string -> string list -> (string * int) list

    val get_offsets_after_substitution : (string * int) list -> Match.Environment.t -> (string * int) list
  end
end

module Language = Language
module Replacement = Replacement
module Statistics = Statistics
module Configuration = Configuration
