open Hack_parallel

open Configuration
open Matchers

val process_paths_for_interactive
  : sequential:bool
  -> f:(input:[> `Path of string ] -> path:string option -> string option * int)
  -> string list
  -> Scheduler.t
  -> Interactive.input list * int

val timed_run
  : (module Matcher)
  -> ?fast_offset_conversion:bool
  -> ?omega:bool
  -> ?rewrite_template:string
  -> ?substitute_in_place:bool
  -> ?rule:Language.Ast.expression list
  -> configuration:Matchers.Configuration.t
  -> template:string
  -> source:string
  -> unit
  -> Match.t list

val with_scheduler : Scheduler.t -> f:(Scheduler.t -> 'a) -> 'a

val with_timeout : int -> [< `Path of string | `Paths of string | `String of string | `Zip of string * Zip.entry list ] -> f:(unit -> 'a list) -> 'a list

val run : Command_configuration.t -> unit
