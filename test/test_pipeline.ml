open Core
open Hack_parallel

open Pipeline
open Command_configuration
open Matchers

let matcher = (module Generic : Matchers.Matcher)

let configuration =
  { sources = `String "source"
  ; specifications = []
  ; exclude_directory_prefix = "."
  ; file_filters = None
  ; run_options =
      { sequential = true
      ; verbose = false
      ; match_timeout = 3
      ; number_of_workers = 4
      ; dump_statistics = false
      ; substitute_in_place = true
      ; disable_substring_matching = false
      }
  ; output_printer = (fun _ -> ())
  ; interactive_review = None
  }

let%expect_test "interactive_paths" =
  let _, count =
    let scheduler = Scheduler.create ~number_of_workers:0 () in
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
    try Pipeline.run matcher configuration; "passed"
    with _exc -> "Not a tty"
  in
  print_string result;
  [%expect_exact {|Not a tty|}]
