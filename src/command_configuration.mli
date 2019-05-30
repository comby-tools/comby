open Core

module Printer : sig
  type printable_result =
    | Matches of
        { source_path : string option
        ; matches : Match.t list
        }
    | Replacements of
        { source_path : string option
        ; replacements : Replacement.t list
        ; result : string
        ; source_content : string
        }

  type t = printable_result -> unit
end

type output_options =
  { json_pretty : bool
  ; json_lines : bool
  ; in_place : bool
  ; stdin : bool
  ; output_diff : bool
  }

type anonymous_arguments =
  { match_template : string
  ; rewrite_template : string
  ; extensions : string list option
  }

type user_input_options =
  { rule : string
  ; specification_directories : string list option
  ; anonymous_arguments : anonymous_arguments option
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
  }

val create : user_input -> t Or_error.t
