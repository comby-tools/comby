open Core_kernel

module Language = struct
  module Syntax = struct
    type escapable_string_literals =
      { delimiters : string list
      ; escape_character : char
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

  let sorts () = [ Everything; Expression; Alphanum; Non_space; Line; Blank; Regex ]
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

module External = struct
  type t = name:string -> filepath:string -> line:int -> column:int -> string option

  module type S = sig
    val handler : t
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
    | External of string
  [@@deriving sexp]

  type syntax =
    { variable : string (* E.g., x *)
    ; pattern : string (* E.g., the entire :[x] part *)
    ; offset : int
    ; kind : kind (* The kind of hole, to inform substitution *)
    }
  [@@deriving sexp]

  type atom =
    | Hole of syntax
    | Constant of string
  [@@deriving sexp]

  type t = atom list [@@deriving sexp]

  module type S = sig
    module Matching : sig
      val hole_parsers : (Hole.sort * string Vangstrom.t) list
    end

    val parse : string -> t
    val variables : string -> syntax list
    val to_string : t -> string
    val substitute : ?filepath:string -> t -> Match.Environment.t -> string * Match.Environment.t
  end
end

module Ast = struct
  type atom =
    | Template of Template.t
    | String of string
  [@@deriving sexp]

  type antecedent = atom [@@deriving sexp]

  type expression =
    | True
    | False
    | Option of string
    | Equal of atom * atom
    | Not_equal of atom * atom
    | Match of atom * (antecedent * consequent) list
    | Rewrite of atom * (antecedent * atom)

  and consequent = expression list [@@deriving sexp]
end

module Rule = struct
  type t = Ast.expression list [@@deriving sexp]

  module type S = sig
    val create : string -> (Ast.expression list, Error.t) result
  end
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
