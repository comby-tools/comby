open Configuration
open Command_input

type output =
  | Matches of (Match.t list * int)
  | Replacement of (Replacement.t list * string * int)
  | Nothing

val process_single_source
  :  (module Matchers.Matcher.S)
  -> ?sequential:bool
  -> ?omega:bool
  -> ?fast_offset_conversion:bool
  -> ?substitute_in_place:bool
  -> ?verbose:bool
  -> ?timeout:int
  -> ?metasyntax:Matchers.Metasyntax.t
  -> Matchers.Configuration.t
  -> single_source
  -> Specification.t
  -> output

val execute
  :  (module Matchers.Matcher.S)
  -> ?substitute_in_place:bool
  -> ?timeout:int
  -> ?metasyntax:Matchers.Metasyntax.t
  -> ?configuration:Matchers.Configuration.t
  -> single_source
  -> Specification.t
  -> output

val with_timeout : int -> Command_input.single_source -> f:(unit -> 'a list) -> 'a list

val run : Command_configuration.t -> unit
