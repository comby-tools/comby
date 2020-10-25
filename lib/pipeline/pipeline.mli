open Hack_parallel

open Configuration
open Matchers

val process_paths_for_interactive
  : sequential:bool
  -> f:(input:Command_input.single_source -> path:string option -> string option * int)
  -> string list
  -> Scheduler.t
  -> Interactive.input list * int

val timed_run
  : (module Matcher)
  -> ?fast_offset_conversion:bool
  -> ?omega:bool
  -> ?substitute_in_place:bool
  -> configuration:Matchers.Configuration.t
  -> source:string
  -> specification:Specification.t  -> unit
  -> Match.t list

val with_scheduler : Scheduler.t -> f:(Scheduler.t -> 'a) -> 'a

val with_timeout : int -> Command_input.single_source -> f:(unit -> 'a list) -> 'a list

val run : Command_configuration.t -> unit
