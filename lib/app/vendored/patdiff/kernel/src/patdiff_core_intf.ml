open! Core_kernel
open! Import

module type S = sig
  (** [diff ~context ~keep_ws ~prev ~next] uses [Patience_diff.String] to get a list of
      hunks describing the comparison between [prev] and [next]. *)
  val diff
    :  context:int
    -> line_big_enough:int
    -> keep_ws:bool
    -> prev:string array
    -> next:string array
    -> Hunks.t

  (** [refine hunks] maps each [Range.Replace (prev, next)] in [hunks] to a diff of [prev]
      against [next]. *)
  val refine
    :  rules:Format.Rules.t
    -> produce_unified_lines:bool
    -> output:Output.t
    -> keep_ws:bool
    -> split_long_lines:bool
    -> interleave:bool
    -> word_big_enough:int
    -> Hunks.t
    -> Hunks.t

  val explode
    :  string array
    -> keep_ws:bool
    -> [ `Newline of int * string option | `Word of string ] array

  (** Print a hunk list, usually from [diff] or [refine] *)
  val print
    :  file_names:File_name.t * File_name.t
    -> rules:Format.Rules.t
    -> output:Output.t
    -> location_style:Format.Location_style.t
    -> Hunks.t
    -> unit

  (** Output a hunk list, usually from [diff] or [refine], to a string *)
  val output_to_string
    :  ?print_global_header:bool
    -> file_names:File_name.t * File_name.t
    -> rules:Format.Rules.t
    -> output:Output.t
    -> location_style:Format.Location_style.t
    -> Hunks.t
    -> string

  (** Iter along the lines of the diff and the breaks between hunks. Offers more flexibility
      regarding what the caller wants to do with the lines *)
  val iter_ansi
    :  rules:Format.Rules.t
    -> f_hunk_break:(int * int -> int * int -> unit)
    -> f_line:(string -> unit)
    -> Hunks.t
    -> unit


  (** Runs the equivalent of the command line version of patdiff on two given contents
      [prev] and [next].  Uses [Patience_diff.String]. *)
  val patdiff
    :  ?context:int
    -> ?keep_ws:bool
    -> ?rules:Format.Rules.t
    -> ?output:Output.t
    -> ?produce_unified_lines:bool
    -> ?split_long_lines:bool
    -> ?print_global_header:bool
    -> ?location_style:Format.Location_style.t
    -> ?interleave:bool
    -> ?line_big_enough:int
    -> ?word_big_enough:int
    -> prev:Diff_input.t
    -> next:Diff_input.t
    -> unit
    -> string
end

module type Output_impls = sig
  val implementation : Output.t -> (module Output.S)
  val console_width : unit -> int Or_error.t
end

module type Patdiff_core = sig
  module type S = S

  val default_context : int
  val default_line_big_enough : int
  val default_word_big_enough : int

  (** [remove_ws] calls String.strip and replaces whitespace with " " *)
  val remove_ws : string -> string

  module Private : sig
    module Make (Output_impls : Output_impls) : S
  end

  module Without_unix : S
end
