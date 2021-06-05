open Configuration
open Command_configuration

open Comby_kernel
open Matchers

let configuration (module E : Engine.S) =
  { matcher = (module E.Generic : Matchers.Matcher.S)
  ; sources = `String "source"
  ; specifications = []
  ; run_options =
      { verbose = false
      ; match_timeout = 3
      ; dump_statistics = false
      ; disable_substring_matching = false
      ; fast_offset_conversion = false
      ; match_newline_toplevel = false
      ; bound_count = None
      ; compute_mode = `Sequential
      }
  ; output_printer = (fun _ -> ())
  ; interactive_review = None
  ; metasyntax = None
  ; substitute_in_place = true
  }

(* TODO restore this, can't access the Parallel_hack module *)
(*
let%expect_test "interactive_paths" =
  let _, count =
    let scheduler = Scheduler.create ~number_of_workers:1 () in
    Pipeline.with_scheduler scheduler ~f:(
      Pipeline.process_paths_for_interactive
        ~sequential:false
        ~f:(fun ~input: _ ~path:_ -> (None, 0)) [])
  in
  print_string (Format.sprintf "%d" count);
  [%expect_exact {|0|}]

let%expect_test "launch_editor" =
  let configuration =
    { configuration
      with interactive_review =
             Some
               { editor = "vim"
               ; default_is_accept = true
               }
    }
  in
  let result =
    try Pipeline.run configuration; "passed"
    with _exc -> "Not a tty"
  in
  print_string result;
  [%expect_exact {|Not a tty|}]
*)
