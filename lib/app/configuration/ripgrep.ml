open Core
open Lwt

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let run ~pattern ~args =
  let options = ["--files-with-matches"; "--multiline"] in
  let pattern = Format.sprintf {|'%s'|} pattern in
  let command = ("rg" :: options @ args @ [pattern]) |> String.concat ~sep:" " in
  if debug then Format.printf "Executing: %s@." command;
  let lwt_command = Lwt_process.shell command in
  let recv proc =
    let ic = proc#stdout in
    Lwt.finalize
      (fun () -> Lwt_io.read ic)
      (fun () -> Lwt_io.close ic)
  in
  let f () =
    Lwt_process.with_process_in lwt_command (fun proc ->
        recv proc >>= fun result ->
        proc#status >>= function
        | WEXITED v when v <> 0 -> return @@ Or_error.errorf "Error executing rg, exit status %d." v
        | _ -> return (Ok (String.split ~on:'\n' result |> List.filter ~f:(String.(<>) ""))))
  in
  try Lwt_main.run (f ()) with Sys.Break -> exit 0
