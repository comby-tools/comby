include module type of Comby_kernel

(** {2 Pipeline}

    Exposes top level functions for running matchers and generating
    replacements. For finer control over matching and rewriting, use Matcher.all
    to generate matches, and then process matches with Rewrite functions. *)
module Pipeline : sig
  (** Source inputs for a pipeline is either a string, or a file path. *)
  type single_source =
    | Path of string
    | String of string

  (** The output of running a specification. Matches are a list of matches and
      the number of matches found. A Replacement consists of the list of
      replacement fragments, the rewrite output, and the number of replacements
      made. *)
  type output =
    | Matches of (Comby_kernel.match' list * int)
    | Replacement of (Comby_kernel.Replacement.t list * string * int)
    | Nothing

  (** [execute matcher subst timeout metasyntax fresh config source spec] runs a
      [matcher] on [source] for [spec] parameterized by [config]. [subst] sets
      whether rewrite output should substitute rewritten values in place.
      [timeout] specifies a timeout in seconds (default 3). If [metasyntax] is
      defined, rewrite operations will respect custom metasyntax definitions.
      Note that [metasyntax] here does not affect matching: [matcher] should be
      defined with a metasyntax definition if desired. A custom [fresh] variable
      generator may supply values to use for substitution; see
      [Rewrite.substitute] for more. *)
  val execute
    :  (module Matchers.Matcher.S)
    -> ?timeout:int
    -> ?metasyntax:Matchers.metasyntax
    -> ?fresh:(unit -> string)
    -> ?configuration:Matchers.configuration
    -> single_source
    -> Matchers.specification
    -> output
end

module Regex : sig
  val to_regex : Matchers.specification -> string
end
