open Core

(* [Command] treats the first space-separated word as part of an argument.
   Without the space prefix, the readme will have the following form in -help output:
   [-readme Display]              documentation for the configuration file and
*)
let doc = " Display documentation for the configuration file and other help"

let main () =
  protectx
    (Filename.temp_file "patdiff" ".man")
    ~f:(fun fn ->
      let readme = Text.Readme.readme in
      Out_channel.write_lines fn [ readme ];
      ignore
        (Unix.system (sprintf "groff -Tascii -man %s|less" fn) : Unix.Exit_or_signal.t))
    ~finally:(fun fn -> Exn.handle_uncaught (fun () -> Unix.unlink fn) ~exit:false)
;;
