open Core

module Printer : sig
  type t =
    | Match_printer of (string option -> Match.t list -> unit)
    | Rewrite_printer of unit
end

type output_options =
  { json_pretty : bool
  ; json_lines : bool
  ; in_place : bool
  ; stdin : bool
  ; output_diff : bool
  }

type user_input_options =
  { rule : string
  ; specification_directories : string list option
  ; anonymous_arguments : (string * string * string list list option) option
  ; file_extensions : string list option
  ; zip_file : string option
  ; match_only : bool
  ; target_directory : string
  }

type run_options =
  { sequential : bool
  ; verbose : bool
  ; match_timeout : int
  ; number_of_workers : int
  ; dump_statistics : bool
  }

type user_input =
  { input_options : user_input_options
  ; run_options : run_options
  ; output_options : output_options
  }

type t =
  { sources : Command_input.t
  ; specifications : Specification.t list
  ; file_extensions : string list option
  ; run_options : run_options
  ; output_printer : Printer.t
  ; output_options : output_options
  }

val create : user_input -> t Or_error.t

val fake_glob_file_extensions : string list -> string list
