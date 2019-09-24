open Core
open Lwt
open Lwt_react

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

module Diff = struct
  open Patdiff_lib
  open Patience_diff_lib
  open Configuration

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
      Patdiff_core.refine
        ~rules
        ~output
        ~keep_ws
        ~produce_unified_lines
        ~split_long_lines
        ~interleave
        hunks
        ~word_big_enough

  let get_hunks config prev next =
    let lines { Patdiff_core.name = _; text } = String.split_lines text |> Array.of_list in
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
      { output; rules; location_style; _ }
      ?print_global_header
      ~(prev : Patdiff_core.diff_input)
      ~(next : Patdiff_core.diff_input) =
    Patdiff_core.output_to_string
      hunks
      ?print_global_header
      ~file_names:(prev.name, next.name)
      ~output
      ~rules
      ~location_style

  let apply_style ~with_style hunks prev next =
    let with_style =
      match with_style with
      | `Pretty context -> Diff_configuration.terminal ~context ()
      | `Plain -> Diff_configuration.plain ()
    in
    let one_hunk = stylize_hunks with_style hunks in
    hunk_to_string one_hunk ~print_global_header:true with_style ~prev ~next

end

module M = struct
  class read_line term prompt = object(self)
    inherit LTerm_read_line.read_line () as super
    inherit [Zed_string.t] LTerm_read_line.term term

    method! show_box = false

    method! send_action action =
      super#send_action action

    initializer
      self#set_prompt begin
        (S.const (LTerm_text.of_utf8 prompt))
      end
  end
end

let clear_screen terminal =
  LTerm.clear_screen terminal >>= fun () ->
  LTerm.with_context terminal (fun context ->
      Lwt_io.write (LTerm.context_oc context) "\027[H")

let handle_patch_errors terminal = function
  | Lwt_unix.WEXITED 0 -> return `Not_ok
  | WEXITED e
  | WSIGNALED e
  | WSTOPPED e ->
    clear_screen terminal >>= fun () ->
    let hint =
      if e = 127 then
        "Maybe the 'patch' command is not on your path?\n"
      else
        ""
    in
    let prompt =
      Format.sprintf
        "Error attempting patch, command exited with %d.\n\
         %s\
         Press any key to continue, or exit now (Ctrl-C).\n" e hint in
    (new M.read_line terminal prompt)#run >>= fun _ ->
    return `Ok

let apply_patch hunk_patch =
  let cmd = Lwt_process.shell "patch -p 0" in
  (return (Lwt_process.open_process_full cmd)) >>= fun process ->
  Lwt_io.write_line process#stdin hunk_patch >>= fun () ->
  Lwt_io.close process#stdin >>= fun () ->
  Lwt_io.read process#stdout >>= fun stdout ->
  Lwt_io.read process#stderr >>= fun stderr ->
  (if debug then LTerm.printf "[debug] %s,%s\n" stdout stderr else return ()) >>= fun () ->
  process#close

let drop_into_editor editor path ~at_line =
  let command = Format.sprintf "%s +%d %s" editor at_line path in
  Lwt_unix.system command >>= fun _ -> return ()

let process_input hunk_patch prev_start next_start terminal editor path ~continue =
  let rec try_again () =
    let prompt =
      "Accept change (\
       y = yes, \
       n = no [default], \
       e = edit original, \
       E = apply+edit, \
       q = quit)?\n"
    in
    (new M.read_line terminal prompt)#run >>= fun key_pressed ->
    match Zed_string.to_utf8 key_pressed with
    | "n" | "" ->
      continue ()
    | "e" ->
      drop_into_editor editor path ~at_line:prev_start
      >>= continue
    | "y" ->
      apply_patch hunk_patch
      >>= handle_patch_errors terminal
      >>= fun _ -> continue ()
    | "E" ->
      apply_patch hunk_patch
      >>= handle_patch_errors terminal
      >>= begin function
        | `Not_ok -> continue ()
        | `Ok ->
          drop_into_editor editor path ~at_line:next_start
          >>= continue
      end
    | "q" ->
      raise Sys.Break
    | _ ->
      LTerm.printl "Uh, I don't know that one. Try again."
      >>= try_again
  in
  try_again ()

type input =
  { path : string
  ; rewritten_source : string
  }

let run editor count rewrites =
  let thread () =
    Lazy.force LTerm.stdout >>= fun terminal ->
    let size = List.length rewrites in
    let text =
      let open LTerm_style in
      let open LTerm_text in
      [S"There "] @
      (if count = 1 then [S"is "; B_fg green; S"1"; E_fg; S" match"]
       else [S"are "; B_fg green; S(Format.sprintf "%d" count); E_fg; S" matches"]) @
      [S" in total, "] @
      (if size = 1 then [S"in "; B_fg green; S"1"; E_fg; S" file"]
       else [S"spread across "; B_fg green; S(Format.sprintf "%d" size); E_fg; S" files"]) @
      [S" to review.\n\
         Press "; B_fg green; S"any key"; E_fg;
       S" to continue on this patching adventure \
         ("; B_fg red; S"Ctrl-C to cancel"; E_fg; S")."]
    in
    clear_screen terminal >>= fun () ->
    LTerm.printls (LTerm_text.(eval text)) >>= fun () ->
    (new M.read_line terminal "")#run >>= fun _ ->
    let do_one_file path rewritten_source =
      let open Patdiff_lib in
      let source_content = In_channel.read_all path in
      let prev = Patdiff_core.{ name = path; text = source_content } in
      let next = Patdiff_core.{ name = path; text = rewritten_source } in
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
          clear_screen terminal >>= fun () ->
          LTerm.printl hunk_pretty >>= fun () ->
          let prev_start = hunk.prev_start + context in
          let next_start = hunk.next_start + context in
          process_input
            ~continue:(fun () -> next_hunk hunks)
            hunk_patch
            prev_start
            next_start
            terminal
            editor
            path
      in
      next_hunk rev_hunks
    in
    Lwt_list.iter_s (fun { path; rewritten_source } -> do_one_file path rewritten_source) rewrites
  in
  try Lwt_main.run (thread ()) with Sys.Break -> exit 0
