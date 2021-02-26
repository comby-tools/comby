open! Core
open! Import
include Patdiff_kernel.Patdiff_core

include Private.Make (struct
    let implementation : Output.t -> (module Output.S) = function
      | Ansi -> (module Patdiff_kernel.Ansi_output)
      | Ascii -> (module Patdiff_kernel.Ascii_output)
      | Html -> (module Html_output)
    ;;

    let console_width () =
      if am_running_test
      then Ok 80
      else
        let open Or_error.Let_syntax in
        let%bind get_size = Linux_ext.get_terminal_size in
        let%map _, width = Or_error.try_with (fun () -> get_size `Controlling) in
        width
    ;;
  end)
