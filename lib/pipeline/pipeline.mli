open Configuration
open Command_input

type processed_source_result =
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
  -> Matchers.Configuration.t
  -> single_source
  -> Specification.t
  -> processed_source_result

val execute
  :  (module Matchers.Matcher.S)
  -> ?substitute_in_place:bool
  -> ?timeout:int
  -> Matchers.Configuration.t
  -> single_source
  -> Specification.t
  -> processed_source_result

val with_timeout : int -> Command_input.single_source -> f:(unit -> 'a list) -> 'a list

val run : Command_configuration.t -> unit
