open Core

let () =
  let result = Result.try_with (fun () -> Command.run Compare.command) in
  match result with
  | Ok () -> ()
  | Error exn ->
    eprintf "%s\n%!" (Exn.to_string exn);
    exit 2
;;
