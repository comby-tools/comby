open Configuration
open Command_input

type processed_source_result =
  | Matches of (Match.t list * int)
  | Replacement of (Replacement.t list * string * int)
  | Nothing

val process_single_source
  :  (module Matchers.Matcher.S)
  -> sequential:bool
  -> omega:bool
  -> fast_offset_conversion:bool
  -> substitute_in_place:bool
  -> verbose:bool
  -> timeout:int
  -> Matchers.Configuration.t
  -> single_source
  -> Specification.t
  -> processed_source_result

val timed_run
  : (module Matchers.Matcher.S)
  -> ?fast_offset_conversion:bool
  -> ?omega:bool
  -> ?substitute_in_place:bool
  -> configuration:Matchers.Configuration.t
  -> source:string
  -> specification:Specification.t  -> unit
  -> Match.t list

val with_timeout : int -> Command_input.single_source -> f:(unit -> 'a list) -> 'a list

val run : Command_configuration.t -> unit
