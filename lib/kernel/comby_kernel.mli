(** {2 Match}

    A match is a result of matching a template pattern in an input source.

    A match comprises:
    (1) an environment of metavariables to values, where values have an associated range.
    (2) the entire string matched by a match template.
    (3) the range of the entire matched string. *)
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

  (** {3 Environment}

      A match environment maps metavariables to values for a given match. *)
  module Environment : sig
    type t
    [@@deriving eq]

    val to_yojson : t -> Yojson.Safe.json
    val of_yojson : Yojson.Safe.json -> (t, string) Result.t

    (** [create] creates a new, empty environment *)
    val create : unit -> t

    (** [vars env] returns all metavariables for this environment. *)
    val vars : t -> string list

    (** [add range env var value] adds a metavariable [var] to [env] with value
        [value] and range [range]. If [var] already exists, this function has no
        effect and returns the existing environment. *)
    val add : ?range:range -> t -> string -> string -> t

    (** [lookup env ar] returns the value assocated with a metavariable [var]. *)
    val lookup : t -> string -> string option

    (** [update env var value] updates the [value] of a metavariable [var] in [env]. If
        the metavariable does not exist, the entry is added. *)
    val update : t -> string -> string -> t

    (** [lookup_range env var] returns the range associated with metavariable [var] in [env]. *)
    val lookup_range : t -> string -> range option

    (** [update_range env var value range] updates [env] with the [range]
        associated with the [value] of [var] *)
    val update_range : t -> string -> range -> t

    val equal : t -> t -> bool

    val copy : t -> t

    val merge : t -> t -> t

    val to_string : t -> string

    val exists : t -> string -> bool
  end

  type environment = Environment.t

  (** A match t is:
      (1) an environment of metavariables to values, where values have an associated range.
      (2) the entire string matched by a match template.
      (3) the range of the entire matched string. *)
  type t =
    { range : range
    ; environment : environment
    ; matched : string
    }

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) Result.t

  (** [create] creates a new match with empty range by default. *)
  val create : ?range:range -> unit -> t

  (** [convert_offset] populates line and column information for a [source] file
      associated with the match (by default, only the offset is computed
      matches). For matches [fast] is an experimental option that uses a binary
      search to perform the conversion quickly. *)
  val convert_offset : fast:bool -> source:string -> t -> t

  (** [pp] is a grep-like formatted printer for matches. It accepts a (optional
      file path * match list) *)
  val pp : Format.formatter -> string option * t list -> unit

  (** [pp] is a JSON formatted printer for (optional file path * match list).
      One line printed per match. *)
  val pp_json_lines : Format.formatter -> string option * t list -> unit

  val pp_match_count : Format.formatter -> string option * t list -> unit
end

type match' = Match.t

(** {2 Replacement}

    Defines the result of a rewrite operation. *)
module Replacement : sig
  (** A replacement consists of the replaced range, the replacement content, and
      the environment associated with the replacement content. *)
  type t =
    { range : Match.range
    ; replacement_content : string
    ; environment : Match.environment
    }

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) Result.t

  val to_json
    :  ?path:string
    -> ?replacements:t list
    -> ?rewritten_source:string
    -> diff:string
    -> unit
    -> Yojson.Safe.json

  (** A replacement result is the rewritten source, and the replacement
      fragments. *)
  type result =
    { rewritten_source : string
    ; in_place_substitutions : t list
    }

  val result_to_yojson : result -> Yojson.Safe.json

end

type replacement = Replacement.result

(** {2 Matchers}

    Defines modules for matching a pattern in input sources to produce
    matches. *)
module Matchers : sig

  (** {3 Configuration}

      Defines some parameters for changing match behavior. *)
  module Configuration : sig
    type t

    type match_kind =
      | Exact
      | Fuzzy

    (** [create] creates a new configuration based for some parts of matching
        behavior. These options are for uncommon use cases and the defaults are
        usually the right choice.

        - [disable_substring_matching] determines whether a pattern like "a"
          matches inside a substring, or whole words. Default: [false].

        - [match_kind] determines whether a match template can match anywhere
          inside a source file (Fuzzy), or must match it exactly (Exact). Default:
          [Fuzzy].

        - [significant_whitespace] is currently has no effect.

        - [match_newline_toplevel] determines whether matching should terminate
          on newlines if a hole is not specified inside a recognized block syntax.
          Default: [false].

        - [fresh] is a generator for fresh values, used to evaluate equality
          relations. *)
    val create
      :  ?disable_substring_matching:bool
      -> ?match_kind:match_kind
      -> ?significant_whitespace:bool
      -> ?match_newline_toplevel:bool
      -> ?fresh:(unit -> string)
      -> unit
      -> t
  end

  type configuration = Configuration.t

  (** {3 Hole}

      The kinds of holes with associated matching behavior:

      - [Everything] is the default hole that matches across newlines and within
        the bounds of code blocks or strings, and lazily between prefix and suffix
        strings in a given template.

      - [Expression] matches expression-like syntax that corresponds to
        contiguous well-formed code structures or strings that are whitespace
        separated.

      - [Alphanum] matches alphanumeric characters and underscore.

      - [Non_space] matches one or more alphanumeric characters and punctuation
        like ., ;, and - that do not affect balanced syntax. Language dependent.

      - [Line] matches zero or more characters up to a newline, including the
        newline.

      - [Blank] matches one or more spaces or tabs.

      - [Regex] matches a string based on a regular expression. *)
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

  (** {3 Metasyntax}

      Defines the metasyntax recognized in templates and associates the
      metasyntax with the matching behavior of holes. *)
  module Metasyntax : sig
    (** aliases where a match of the string [pattern] maps to [match_template] and [rule]. *)
    type alias =
      { pattern : string
      ; match_template : string
      ; rule : string option
      }

    (** A hole definition should comprise either a string prefix, suffix, or
        both which encloses an variable identifier. See example below. *)
    type hole_definition =
      | Delimited of string option * string option
      | Reserved_identifiers of string list

    (** Defines syntax definitions for holes. Zero or more Hole sorts, excluding
        [Regex] should have an associated [hole_definition]. The [Regex] hole
        must define a prefix, separator, and  suffix. See example below. *)
    type hole_syntax =
      | Hole of Hole.sort * hole_definition
      | Regex of string * char * string

    (** A metasyntax comprises:

        - [identifier] where the list of characters are allowed in identifiers.
          For example, to allow only contiguous capitalized letters as recognized
          identifiers within some hole syntax, use:

          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

        - [syntax] with one or more hole definitions. For example, the default
          metasyntax for the [Everything] hole is defined as:

          [ Hole (Everything, Delimited (Some ":[", Some "]")) ]

          A Regex hole must define a prefix, separator, and suffix. The current
          convention is taken to parse Regex holes as:

          <prefix><identifier><separator><regular expression><suffix>

          A separator is required to syntactically distinguish arbitrary
          identifier syntax from regular exressions. A suffix is required to
          syntactically distinguish when to stop parsing a regular expression and
          resume parsing the rest of the template. *)
    type t =
      { syntax : hole_syntax list
      ; identifier : string
      ; aliases : alias list
      }

    val to_yojson : t -> Yojson.Safe.json
    val of_yojson : Yojson.Safe.json -> (t, string) Result.t

    (** A module signature for metasyntax to parameterize a matcher *)
    module type S = sig
      val syntax : hole_syntax list
      val identifier : string
      val aliases : alias list
    end

    (** A module representing the default metasyntax *)
    module Default : S

    (** The default metasyntax. It is defined as:

        let default_syntax =
          [ Hole (Everything, Delimited (Some ":[", Some "]"))
          ; Hole (Expression, Delimited (Some ":[", Some ":e]"))
          ; Hole (Alphanum, Delimited (Some ":[[", Some "]]"))
          ; Hole (Non_space, Delimited (Some ":[", Some ".]"))
          ; Hole (Line, Delimited (Some ":[", Some "\\n]"))
          ; Hole (Blank, Delimited (Some ":[ ", Some "]"))
          ; Regex (":[", '~', "]")
          ]

        let default_identifier =
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    *)
    val default_metasyntax : t

    (** A JSON representation of the metasyntax defintion *)
    val json : t -> string

    (** [create definition] creates a metasyntax module from a definition *)
    val create : t -> (module S)

    (** [default] returns the default metasyntax module *)
    val default : (module S)
  end

  type metasyntax = Metasyntax.t

  module External : sig
    type t = name:string -> filepath:string -> line:int -> column:int -> string option

    module type S = sig
      val handler : t
    end

    (** A module representing the default external handler *)
    module Default : S
  end

  (** {3 Template}

      Parse a template based on metasynax *)

  module Template : sig
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

    type syntax =
      { variable : string
      ; pattern : string
      ; offset : int
      ; kind : kind
      }
    [@@deriving sexp]

    type atom =
      | Hole of syntax
      | Constant of string
    [@@deriving sexp]

    type t = atom list
    [@@deriving sexp]

    module Make : Metasyntax.S -> External.S -> sig
        val parse : string -> t
        val variables : string -> syntax list
      end
  end

  (** {3 AST}

      Defines a rule AST. *)
  module Ast : sig
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

  (** {3 Matcher}

      Defines the functions that a matcher can perform. *)
  module rec Matcher : sig
    module type S = sig
      (** [all conf nested template source] finds all matches of [template] in
          [source]. If [nested] is true, template matching will descend
          recursively on matched content. *)
      val all
        :  ?configuration:configuration
        -> ?filepath:string
        -> ?rule:Rule.t
        -> template:string
        -> source:string
        -> unit
        -> match' list

      (** [first conf shift template source] finds the first match of [template] in [source] starting
          at position [shift] (default 0). *)
      val first
        :  ?configuration:configuration
        -> ?shift:int
        -> ?filepath:string
        -> string
        -> string
        -> match' Core_kernel.Or_error.t

      (** [name] returns the name of this matcher (e.g., "C", "Go", etc.). *)
      val name : string

      (** [extensions] returns the file extensions associated with the language
          that this matcher supports (e.g., ".c" and "h" for C). *)
      val extensions : string list

      (** [set_rewrite_template] is currently unused. *)
      val set_rewrite_template : string -> unit
    end
  end

  (** {2 Rule}

      Defines types and operations for match rules. *)
  and Rule : sig

    type t = Ast.expression list
    [@@deriving sexp]

    type options =
      { nested : bool
      }

    val create
      :  ?metasyntax:metasyntax
      -> ?external_handler:External.t
      -> string
      -> t Core_kernel.Or_error.t

    val options : t -> options

    type result

    (** [sat result] returns true if a result of a rule is satisfied. *)
    val sat : result -> bool

    val result_env : result -> Match.environment option

    (** [apply matcher substitute_in_place fresh metasyntax rule env] applies a [rule]
        according to some [matcher] for existing matches in [env]. If
        [substitute_in_place] is true, rewrite rules substitute their values in
        place (default true). [fresh] introduces fresh variables for evaluating
        rules. [metasyntax] uses the custom metasyntax definition. *)
    val apply
      :  ?substitute_in_place:bool
      -> ?metasyntax:Metasyntax.t
      -> ?external_handler:External.t
      -> ?filepath:string
      -> match_all:(
          ?configuration:Configuration.t
          -> ?filepath:string
          -> template:string
          -> source:string
          -> unit
          -> Match.t list)
      -> Ast.expression list
      -> Match.Environment.t
      -> result
  end

  type rule = Rule.t

  (** {3 Specification}

      Defines an internal type that represents an atomic operation for matching,
      rule application and rewriting. *)
  module Specification : sig
    type t =
      { match_template : string
      ; rule : Rule.t option
      ; rewrite_template : string option
      }

    (** [create rewrite_template rule match_template] creates a new specification.
        If [rule] is supplied, it will be applied to matches of [match_template].
        If [rewrite_template] is supplied, running a specification will return
        replacements rather than just matches (see [process_single_source] below).
    *)
    val create : ?rewrite_template:string -> ?rule:rule -> match_template:string -> unit -> t

    (** [regex [t] returns a generalized regular expression corresponding to the specification *)
    val to_regex : t -> string
  end

  type specification = Specification.t


  (** {3 Language}

      Language definitions *)
  module Language : sig

    (** {4 Syntax}

        Defines the syntax structures for the target language (C, Go, etc.) that
        are significant for matching. *)
    module Syntax : sig

      (** Defines a set of quoted syntax for strings based on one or more
          delimiters and associated escape chracter.

          E.g., this supports single and double quotes with escape character '\'
          as: { delimiters = [ {|"|}, {|'|} ]; escape_character = '\\' } *)
      type escapable_string_literals =
        { delimiters : string list
        ; escape_character: char
        }

      (** Defines comment syntax as one of Multiline, Nested_multiline with
          associated left and right delimiters, or Until_newline that defines a
          comment prefix. associated prefix. *)
      type comment_kind =
        | Multiline of string * string
        | Nested_multiline of string * string
        | Until_newline of string

      (** Defines syntax as:

          - [user_defined_delimiters] are delimiters treated as code structures
            (parentheses, brackets, braces, alphabetic words) -
            [escapable_string_literals] are escapable quoted strings

          - [raw_string literals] are raw quoted strings that have no escape
            character

          - [comments] are comment structures  *)
      type t =
        { user_defined_delimiters : (string * string) list
        ; escapable_string_literals : escapable_string_literals option [@default None]
        ; raw_string_literals : (string * string) list
        ; comments : comment_kind list
        }

      val to_yojson : t -> Yojson.Safe.json
      val of_yojson : Yojson.Safe.json -> (t, string) Result.t

      (** The module signature that defines language syntax for a matcher *)
      module type S = sig
        val user_defined_delimiters : (string * string) list
        val escapable_string_literals : escapable_string_literals option
        val raw_string_literals : (string * string) list
        val comments : comment_kind list
      end
    end

    module Info : sig
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

  module Languages : sig
    module Text : Language.S
    module Paren : Language.S
    module Dyck : Language.S
    module JSON : Language.S
    module JSONC : Language.S
    module GraphQL : Language.S
    module Dhall : Language.S
    module Latex : Language.S
    module Assembly : Language.S
    module Clojure : Language.S
    module Lisp : Language.S
    module Generic : Language.S
    module Bash : Language.S
    module Ruby : Language.S
    module Elixir : Language.S
    module Python : Language.S
    module Html : Language.S
    module Xml : Language.S
    module SQL : Language.S
    module Erlang : Language.S
    module C : Language.S
    module Csharp : Language.S
    module Java : Language.S
    module CSS : Language.S
    module Kotlin : Language.S
    module Scala : Language.S
    module Nim : Language.S
    module Matlab : Language.S
    module Dart : Language.S
    module Php : Language.S
    module Go : Language.S
    module Javascript : Language.S
    module Jsx : Language.S
    module Typescript : Language.S
    module Tsx : Language.S
    module Swift : Language.S
    module Rust : Language.S
    module R : Language.S
    module OCaml : Language.S
    module Reason : Language.S
    module Fsharp : Language.S
    module Pascal : Language.S
    module Julia : Language.S
    module Fortran : Language.S
    module Haskell : Language.S
    module HCL : Language.S
    module Elm : Language.S
    module Zig : Language.S
    module Coq : Language.S
    module Move : Language.S
    module Solidity : Language.S
    module C_nested_comments : Language.S

    val all : (module Language.S) list

    val select_with_extension : string -> (module Language.S) option
  end

  module Engine : sig
    module type S = sig
      module Make : Language.S -> Metasyntax.S -> External.S -> Matcher.S

      (** {4 Supported Matchers} *)
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
      module Matlab : Matcher.S
      module Dart : Matcher.S
      module Php : Matcher.S
      module Go : Matcher.S
      module Javascript : Matcher.S
      module Jsx : Matcher.S
      module Typescript : Matcher.S
      module Tsx : Matcher.S
      module Swift : Matcher.S
      module Rust : Matcher.S
      module R : Matcher.S
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

      (** [all] returns all default matchers. *)
      val all : (module Matcher.S) list

      (** [select_with_extension metasyntax external file_extension] is a
          convenience function that returns a matcher associated with a
          [file_extension]. E.g., use ".c" to get the C matcher. For a full list
          of extensions associated with matchers, run comby -list. If
          [metasyntax] is specified, the matcher will use a custom metasyntax
          definition instead of the default. An experimental [external] callback
          is a general callback for handling external properties in the rewrite
          template. *)
      val select_with_extension
        :  ?metasyntax:Metasyntax.t
        -> ?external_handler:External.t
        -> string
        -> (module Matcher.S) option


      (** [create metasyntax external syntax] creates a matcher for a language
          defined by [syntax]. If [metasyntax] is specified, the matcher will use
          a custom metasyntax definition instead of the default. An experimental
          [external] callback is a general callback for handling external
          properties in the rewrite template. *)
      val create : ?metasyntax:metasyntax -> ?external_handler:External.t -> Language.Syntax.t -> (module Matcher.S)
    end
  end

  (** {3 Alpha Matcher}

      Alpha is the match engine that defines default matchers for languages.
  *)
  module Alpha : Engine.S

  (** {3 Omega Matcher}

      Alternative, partial, experimental match engine.
  *)
  module Omega : Engine.S

  (** {3 Rewrite}

      Defines rewrite operations.  *)
  module Rewrite : sig
    (** [all source metasyntax external fresh rewrite_template matches] substitutes
        [rewrite_template] with each match in [matches] to create a rewrite result.
        If [source] is specified, each rewrite result is substituted in-place in
        the source. If [source] is not specified, rewritten matches are
        newline-separated. If [metasyntax] is defined, the rewrite template will
        respect custom metasyntax definitions.

        If the rewrite template contains the syntax :[id()], then it is
        substituted with fresh values. [fresh] may be specified to supply custom
        fresh values. If not specified, fresh variables are generated in increasing
        rank starting with 1, and incremented. See [substitute] for more. *)
    val all
      :  ?source:string
      -> ?metasyntax:metasyntax
      -> ?external_handler:External.t
      -> ?fresh:(unit -> string)
      -> ?filepath:string
      -> rewrite_template:string
      -> match' list
      -> replacement option

    (** [substitute metasyntax external fresh template environment] substitutes
        [template] with the variable and value pairs in the [environment]. It
        returns the result after substitution. If [metasyntax] is defined, the
        rewrite template will respect custom metasyntax definitions.

        The syntax :[id()] is substituted with fresh values. If [fresh] is not
        specified, the default behavior substitutes :[id()] starting with 1, and
        subsequent :[id()] values increment the ID. If [fresh] is set,
        substitutes the pattern :[id()] with the value of fresh () as the hole is
        encountered, left to right. An experimental [external] callback is a
        general callback for handling external properties in the rewrite
        template. *)
    val substitute
      :  ?metasyntax:metasyntax
      -> ?external_handler:External.t
      -> ?fresh:(unit -> string)
      -> ?filepath:string
      -> string
      -> Match.environment
      -> string
  end
end
