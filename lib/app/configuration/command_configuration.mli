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

type interactive_review =
  { editor : string
  ; default_is_accept : bool
  }

type output_options =
  { color : bool
  ; json_lines : bool
  ; json_only_diff : bool
  ; overwrite_file_in_place : bool
  ; diff : bool
  ; stdout : bool
  ; substitute_in_place : bool
  ; count : bool
  ; interactive_review : interactive_review option
  }

type anonymous_arguments =
  { match_template : string
  ; rewrite_template : string
  ; file_filters : string list option
  }

type user_input_options =
  { rule : string
  ; stdin : bool
  ; templates : string list option
  ; anonymous_arguments : anonymous_arguments option
  ; file_filters : string list option
  ; zip_file : string option
  ; match_only : bool
  ; target_directory : string
  ; directory_depth : int option
  ; exclude_directory_prefix : string list
  ; exclude_file_prefix : string list
  ; custom_metasyntax : string option
  ; custom_matcher : string option
  ; override_matcher : string option
  ; regex_pattern : bool
  ; ripgrep_args : string option
  }

type compute_mode =
  [ `Sequential
  | `Hack_parallel of int
  | `Parany of int
  ]

type run_options =
  { verbose : bool
  ; match_timeout : int
  ; dump_statistics : bool
  ; substitute_in_place : bool
  ; disable_substring_matching : bool
  ; omega : bool
  ; fast_offset_conversion : bool
  ; match_newline_toplevel : bool
  ; bound_count : int option
  ; compute_mode : compute_mode
  }

type user_input =
  { input_options : user_input_options
  ; run_options : run_options
  ; output_options : output_options
  }

type t =
  { sources : Command_input.t
  ; specifications : Specification.t list
  ; run_options : run_options
  ; output_printer : Printer.t
  ; interactive_review : interactive_review option
  ; matcher : (module Matchers.Matcher.S)
  ; extension : string option
  ; metasyntax :  Matchers.Metasyntax.t option
  }

val create : user_input -> t Or_error.t
