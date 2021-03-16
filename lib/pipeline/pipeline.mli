open Configuration

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
