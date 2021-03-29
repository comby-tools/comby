open Core

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
end

type match' = Match.t

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
          Default: [false]. *)
    val create
      :  ?disable_substring_matching:bool
      -> ?match_kind:match_kind
      -> ?significant_whitespace:bool
      -> ?match_newline_toplevel:bool
      -> unit
      -> t
  end

  type configuration = Configuration.t

  (** {3 Syntax}

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

  type syntax = Syntax.t

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

    (** A hole definition should comprise either a string prefix, suffix, or
        both which encloses an variable identifier. See example below. *)
    type hole_definition =
        Delimited of string option * string option

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
      }

    (** A module signature for metasyntax to parameterize a matcher *)
    module type S = sig
      val syntax : hole_syntax list
      val identifier : string
    end

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

  (** {3 Matcher}

      Defines the functions that a matcher can perform. *)
  module Matcher : sig
    module type S = sig
      (** [all conf nested template source] finds all matches of [template] in
          [source]. If [nested] is true, template matching will descend
          recursively on matched content. *)
      val all
        :  ?configuration:configuration
        -> ?nested: bool
        -> template:string
        -> source:string
        -> unit
        -> match' list

      (** [first conf shift template source] finds the first match of [template] in [source] starting
          at position [shift] (default 0). *)
      val first
        :  ?configuration:configuration
        -> ?shift:int
        -> string
        -> string
        -> match' Or_error.t

      (** [name] returns the name of this matcher (e.g., "C", "Go", etc.). *)
      val name : string

      (** [extensions] returns the file extensions associated with the language
          that this matcher supports (e.g., ".c" and "h" for C). *)
      val extensions : string list

      (** [set_rewrite_template] is currently unused. *)
      val set_rewrite_template : string -> unit
    end
  end

  (** {3 Alpha Matcher}

      Alpha is the match engine that defines default matchers for languages.
  *)
  module Alpha : sig
    (** [select_with_extension metasyntax file_extension] is a convenience
        function that returns a matcher associated with a [file_extension]. E.g.,
        use ".c" to get the C matcher. For a full list of extensions associated
        with matchers, run comby -list. If [metasyntax] is specified, the matcher
        will use a custom metasyntax definition instead of the default. *)
    val select_with_extension : ?metasyntax:metasyntax -> string -> (module Matcher.S) option

    (** [create metasyntax syntax] creates a matcher for a language defined by
        [syntax]. If [metasyntax] is specified, the matcher will use a custom
        metasyntax definition instead of the default. *)
    val create : ?metasyntax:metasyntax -> syntax -> (module Matcher.S)

    (** [all] returns all default matchers. *)
    val all : (module Matcher.S) list

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
end

(** {2 Rule}

    Defines types and operations for match rules. *)
module Rule : sig
  type t
  type result

  (** [sat result] returns true if a result of a rule is satisfied. *)
  val sat : result -> bool

  (** [result_env] returns a match environment associated with a rule result. *)
  val result_env : result -> Match.environment option

  (** [create] parses and creates a rule. *)
  val create : string -> t Or_error.t

  (** [apply matcher substitute_in_place rule env] applies a [rule] according to
      some [matcher] for existing matches in [env]. If [substitute_in_place] is
      true, rewrite rules substitute their values in place (default true). *)
  val apply
    :  ?matcher:(module Matchers.Matcher.S)
    -> ?substitute_in_place:bool
    -> t
    -> Match.environment
    -> result
end

type rule = Rule.t

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

  (** A replacement result is the rewritten source, and the replacement
      fragments. *)
  type result =
    { rewritten_source : string
    ; in_place_substitutions : t list
    }

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) Result.t
end

type replacement = Replacement.result

(** {2 Rewrite}

    Defines rewrite operations.  *)
module Rewrite : sig
  (** [all source metasyntax sequential rewrite_template matches] substitutes
      [rewrite_template] with each match in [matches] to create a rewrite result.
      If [source] is specified, each rewrite result is substituted in-place in
      the source. If [source] is not specified, rewritten matches are
      newline-separated. If [metasyntax] is defined, the
      rewrite template will respect custom metasyntax definitions.

      If the rewrite template contains the syntax :[id()], then it is
      substituted with fresh values. [sequential] determines whether fresh values
      are monitonically increasing or a random hash. See [substitute] for more. *)
  val all
    :  ?source:string
    -> ?metasyntax:Matchers.metasyntax
    -> ?sequential:bool
    -> rewrite_template:string
    -> match' list
    -> replacement option

  (** [substitute metasyntax sequential template environment] substitutes
      [template] with the variable and value pairs in the [environment]. It
      returns the result after substitution, and the list of variables in
      [environment] that were substituted for. If [metasyntax] is defined, the
      rewrite template will respect custom metasyntax definitions.

      The syntax :[id()] is substituted with fresh values. If [sequential] is
      true, it substitutes :[id()] starting with 1, and subsequent :[id()] values
      increment the ID. Otherwise if [sequential] is false, it substitutes the
      pattern :[id()] with a fresh hex string based on the last 48-bit part of a
      UUID v3 identifier. *)
  val substitute : ?metasyntax:Matchers.metasyntax -> ?sequential:bool -> string -> Match.environment -> (string * string list)
end

(** {2 Pipeline}

    Exposes top level functions for running matchers and generating
    replacements. For finer control over matching and rewriting, use Matcher.all
    to generate matches, and then process matches with Rewrite functions. *)
module Pipeline : sig
  (** Source inputs for a pipeline is either a string, or a file path. *)
  type single_source =
    | Path of string
    | String of string

  (** {2 Specification}

      Defines an internal type that represents an atomic operation for matching,
      rule application and rewriting. *)
  module Specification : sig
    type t

    (** [create rewrite_template rule match_template] creates a new specification.
        If [rule] is supplied, it will be applied to matches of [match_template].
        If [rewrite_template] is supplied, running a specification will return
        replacements rather than just matches (see [process_single_source] below).
    *)
    val create
      :  ?rewrite_template:string
      -> ?rule:rule
      -> match_template:string
      -> unit
      -> t
  end

  type specification = Specification.t

  (** The output of running a specification. Matches are a list of matches and
      the number of matches found. A Replacement consists of the list of
      replacement fragments, the rewrite output, and the number of replacements
      made. *)
  type output =
    | Matches of (match' list * int)
    | Replacement of (Replacement.t list * string * int)
    | Nothing

  (** [execute matcher metasyntax subst timeout config source spec] runs a
      [matcher] on [source] for [spec] parameterized by [config].
      [substitute_in_place] sets whether rewrite output should substitute
      rewritten values in place. [timeout] specifies a timeout in seconds
      (default 3). If [metasyntax] is defined, rewrite operations will respect
      custom metasyntax definitions. Note that [metasyntax] here does not affect
      matching: [matcher] should be defined with a metasyntax definition if
      desired. *)
  val execute
    :  (module Matchers.Matcher.S)
    -> ?substitute_in_place:bool
    -> ?timeout:int
    -> ?metasyntax:Matchers.metasyntax
    -> ?configuration:Matchers.configuration
    -> single_source
    -> specification
    -> output
end
