open Core
open Lwt

open Configuration

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

module Diff = struct
  open Patdiff
  open Configuration
  open Patience_diff_lib
  open Patdiff_kernel
  open Patdiff_core

  (* Useful unexposed function Taken from Compare_core. *)
  let compare_lines
      { float_tolerance
      ; context
      ; line_big_enough
      ; ext_cmp
      ; keep_ws
      ; _
      }
      ~prev
      ~next =
    (* Create the diff *)
    let hunks =
      let transform = if keep_ws then Fn.id else Patdiff_core.remove_ws in
      (* Use external compare program? *)
      match ext_cmp with
      | None ->
        Patience_diff.String.get_hunks
          ~transform
          ~context
          ~big_enough:line_big_enough
          ~prev
          ~next
      | Some prog ->
        let compare x y =
          let cmd = sprintf "%s %S %S" prog x y in
          match Unix.system cmd with
          | Ok () -> 0
          | Error (`Exit_non_zero 1) -> 1
          | Error _ -> failwithf "External compare %S failed!" prog ()
        in
        let module P = Patience_diff.Make (struct
            type t = string [@@deriving sexp]

            let hash = String.hash
            let compare = compare
          end)
        in
        P.get_hunks ~transform ~context ~big_enough:line_big_enough ~prev ~next
    in
    match float_tolerance with
    | None -> hunks
    | Some tolerance -> Float_tolerance.apply hunks tolerance ~context

  let stylize_hunks
      { unrefined
      ; rules
      ; output
      ; produce_unified_lines
      ; interleave
      ; word_big_enough
      ; keep_ws
      ; split_long_lines
      ; _
      }
      hunks =
    if unrefined then Patience_diff.Hunks.unified hunks
    else
      Without_unix.refine
        ~rules
        ~output
        ~keep_ws
        ~produce_unified_lines
        ~split_long_lines
        ~interleave
        hunks
        ~word_big_enough

  let get_hunks config prev next =
    let lines { Patdiff.Diff_input.name = _; text } = String.split_lines text |> Array.of_list in
    let hunks =
      Comparison_result.create config
        ~prev
        ~next
        ~compare_assuming_text:(fun config ~prev ~next ->
            compare_lines config ~prev:(lines prev) ~next:(lines next))
    in
    if Comparison_result.has_no_diff hunks then [] else
      match hunks with
      | Hunks h -> h
      | _ -> []

  let hunk_to_string
      hunks
      (style : Patdiff.Configuration.t) (*{ output; rules; location_style; _ }*)
      ?print_global_header
      ~(prev : Patdiff.Diff_input.t)
      ~(next : Patdiff.Diff_input.t)
      () =
    Without_unix.output_to_string
      hunks
      ?print_global_header
      ~file_names:(Fake prev.name, Fake next.name)
      ~output:style.output
      ~rules:style.rules
      ~location_style:style.location_style

  let apply_style ~with_style hunks prev next =
    let with_style =
      match with_style with
      | `Pretty context -> Diff_configuration.terminal ~context ()
      | `Plain -> Diff_configuration.plain ()
    in
    let one_hunk = stylize_hunks with_style hunks in
    hunk_to_string one_hunk with_style ~print_global_header:true ~prev ~next ()

end

let clear_screen () =
  Lwt_io.print "\027[2J" >>= fun () ->
  Lwt_io.print "\027[H"

let handle_editor_errors = function
  | Lwt_unix.WEXITED 0 -> return `Ok
  | WEXITED e | WSIGNALED e | WSTOPPED e ->
    clear_screen () >>= fun () ->
    let message =
      Format.sprintf
        "Error opening editor (error code %d)\n.
         Press any key to continue, or exit now (Ctrl-C).\n" e in
    Lwt_io.print message >>= fun () ->
    Lwt_io.read Lwt_io.stdin >>= fun _input ->
    return `Ok

let handle_patch_errors = function
  | Lwt_unix.WEXITED 0 -> return `Ok
  | WEXITED e
  | WSIGNALED e
  | WSTOPPED e ->
    clear_screen () >>= fun () ->
    let hint =
      if e = 127 then
        "Maybe the 'patch' command is not on your path?\n"
      else
        "Run the command again with DEBUG_COMBY=1 set in the environment for more info.\n"
    in
    let message =
      Format.sprintf
        "Error attempting patch, command exited with %d.\n\
         %s\
         Press any key to continue, or exit now (Ctrl-C).\n" e hint in
    Lwt_io.print message >>= fun _input ->
    Lwt_io.read_line Lwt_io.stdin >>= fun _input ->
    return `Ok

let apply_patch hunk_patch =
  let cmd = Lwt_process.shell "patch -p 0" in
  (return (Lwt_process.open_process_full cmd)) >>= fun process ->
  Lwt_io.write_line process#stdin hunk_patch >>= fun () ->
  Lwt_io.close process#stdin >>= fun () ->
  Lwt_io.read process#stdout >>= fun stdout ->
  Lwt_io.read process#stderr >>= fun stderr ->
  (if debug then Lwt_io.printf "[debug] %s,%s\n" stdout stderr else return ()) >>= fun () ->
  process#close

let drop_into_editor editor path ~at_line =
  let command = Format.sprintf "%s +%d %s" editor at_line path in
  Lwt_unix.system command


let process_input default_is_accept hunk_patch prev_start next_start editor path ~continue =
  let prompt =
    if default_is_accept then
      [ "Accept change ("
      ; "\x1b[32m"; "y = yes"; "\x1b[0m"; "\x1b[1m"; " [default], "; "\x1b[0m"
      ; "\x1b[31m"; "n = no"; "\x1b[0m"; ", "
      ; "\x1b[33m"; "e = edit original"; "\x1b[0m"; ", "
      ; "\x1b[33m"; "E = apply+edit"; "\x1b[0m"; ", "
      ; "q = quit)?"
      ]
    else
      [ "Accept change ("
      ; "\x1b[32m"; "y = yes"; "\x1b[0m"; ", "
      ; "\x1b[31m"; "n = no"; "\x1b[0m"; "\x1b[1m"; " [default], "; "\x1b[0m"
      ; "\x1b[33m"; "e = edit original"; "\x1b[0m"; ", "
      ; "\x1b[33m"; "E = apply+edit"; "\x1b[0m"; ", "
      ; "q = quit)?"
      ]
  in
  let prompt = String.concat prompt in
  Lwt_io.printl prompt >>= fun () ->
  let rec try_again () =
    Lwt_io.read_line Lwt_io.stdin >>= fun input ->
    match input with
    | "y" ->
      apply_patch hunk_patch
      >>= handle_patch_errors
      >>= fun _ -> continue ()
    | "" when default_is_accept ->
      apply_patch hunk_patch
      >>= handle_patch_errors
      >>= fun _ -> continue ()
    | "n" ->
      continue ()
    | "" when not default_is_accept ->
      continue ()
    | "e" ->
      drop_into_editor editor path ~at_line:prev_start
      >>= handle_editor_errors
      >>= fun _ -> continue ()
    | "E" ->
      apply_patch hunk_patch
      >>= handle_patch_errors
      >>= fun _ -> drop_into_editor editor path ~at_line:next_start
      >>= handle_editor_errors
      >>= fun _ -> continue ()
    | "q" ->
      raise Sys.Break
    | _ ->
      Lwt_io.printl "Uh, I don't know that one. Try again."
      >>= try_again
  in
  try_again ()

type input =
  { path : string
  ; rewritten_source : string
  }

let run editor default_is_accept count rewrites =
  let thread () =
    let size = List.length rewrites in
    let text =
      ["There "] @
      (if count = 1 then ["is "; "\x1b[32m"; "1"; "\x1b[0m"; " match"]
       else ["are "; "\x1b[32m"; Format.sprintf "%d" count; "\x1b[0m"; " matches"]) @
      [" in total, "] @
      (if size = 1 then ["in "; "\x1b[32m"; "1"; "\x1b[0m"; " file"]
       else ["spread across "; "\x1b[32m"; Format.sprintf "%d" size; "\x1b[0m"; " files"]) @
      [" to review.\n\
        Press "; "\x1b[32m"; "any key"; "\x1b[0m";
       " to continue on this patching adventure \
        ("; "\x1b[31m"; "Ctrl-C to cancel"; "\x1b[0m"; ")."]
    in
    let text = String.concat text in
    clear_screen () >>= fun () ->
    Lwt_io.printl text >>= fun () ->
    Lwt_io.read_line Lwt_io.stdin >>= fun _input ->
    let do_one_file path rewritten_source =
      let open Patdiff in
      let source_content = In_channel.read_all path in
      let path = Filename_extended.make_relative path in
      let prev = Diff_input.{ name = path; text = source_content } in
      let next = Diff_input.{ name = path; text = rewritten_source } in
      let context = 3 in
      let rev_hunks =
        let configuration = Diff_configuration.terminal ~context () in
        Diff.get_hunks configuration prev next |> List.rev
      in
      let rec next_hunk = function
        | [] -> return ()
        | hunk :: hunks ->
          let one_hunk = [hunk] in
          let hunk_pretty, hunk_patch =
            let apply = Diff.apply_style one_hunk prev next in
            apply ~with_style:(`Pretty context), apply ~with_style:`Plain
          in
          clear_screen () >>= fun () ->
          Lwt_io.printl hunk_pretty >>= fun () ->
          let prev_start = hunk.prev_start + context in
          let next_start = hunk.next_start + context in
          process_input
            default_is_accept
            ~continue:(fun () -> next_hunk hunks)
            hunk_patch
            prev_start
            next_start
            editor
            path
      in
      next_hunk rev_hunks
    in
    Lwt_list.iter_s (fun { path; rewritten_source } -> do_one_file path rewritten_source) rewrites
  in
  try Lwt_main.run (thread ()) with Sys.Break -> exit 0
