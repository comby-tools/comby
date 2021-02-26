open Core

let doc = "FILE Write default configuration file"

let main filename =
  let delete =
    if Sys.file_exists_exn filename
    then (
      printf "%s already exists. Overwrite? (y/n) %!" filename;
      let resp = In_channel.input_line In_channel.stdin in
      let resp = Option.value ~default:"" resp in
      let resp = String.lowercase resp in
      match resp with
      | "yes" | "y" -> true
      | _ -> false)
    else true
  in
  if delete
  then (
    try
      Patdiff.Configuration.save_default ~filename;
      printf "Default configuration written to %s\n%!" filename
    with
    | e -> failwithf "Error: %s" (Exn.to_string e) ())
  else (
    printf "Configuration file not written!\n%!";
    exit 1)
;;
