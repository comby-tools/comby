open Comby_kernel
open Configuration
open Command_input

type output =
  | Matches of (Match.t list * int)
  | Replacement of (Replacement.t list * string * int)
  | Nothing

val process_single_source
  :  (module Matchers.Matcher.S)
  -> ?fast_offset_conversion:bool
  -> ?verbose:bool
  -> ?timeout:int
  -> ?metasyntax:Matchers.Metasyntax.t
  -> ?fresh:(unit -> string)
  -> ?substitute_in_place:bool
  -> Matchers.Configuration.t
  -> single_source
  -> Matchers.Specification.t
  -> output

val execute
  :  (module Matchers.Matcher.S)
  -> ?timeout:int
  -> ?metasyntax:Matchers.Metasyntax.t
  -> ?fresh:(unit -> string)
  -> ?configuration:Matchers.Configuration.t
  -> ?substitute_in_place:bool
  -> single_source
  -> Matchers.Specification.t
  -> output

val with_timeout : int -> Command_input.single_source -> f:(unit -> 'a list) -> 'a list
val run : Command_configuration.t -> unit
