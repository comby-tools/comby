open Comby_semantic

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

let lsif_endpoint =
  match Sys.getenv "LSIF_SERVER" with
  | exception Not_found -> "https://sourcegraph.com/.api/graphql"
  | address -> address

let repository_remote () =
  let open Core in
  In_channel.input_all (Unix.open_process_in "git config --get remote.origin.url")

let revision () =
  let open Core in
  In_channel.input_all (Unix.open_process_in "git rev-parse HEAD")

let lsif_hover ~name:_ ~filepath ~line ~column =
  let open Core_kernel in
  try
    let repository =
      repository_remote ()
      |> String.chop_prefix_if_exists ~prefix:"https://"
      |> String.chop_suffix_if_exists ~suffix:"\n"
      |> String.chop_suffix_if_exists ~suffix:".git"
    in
    let revision = revision () |> String.rstrip in
    if debug then Format.printf "Repository remote: %s\nRevision: %s@." repository revision;
    String.chop_prefix_if_exists filepath ~prefix:(Sys.getcwd ())
    |> fun filepath_relative_root ->
    if debug then Format.printf "File relative root: %s@." filepath;
    if debug then Format.printf "Querying type at %d::%d@." line column;
    let context = Lsif.Context.{ repository; lsif_endpoint; revision; formatting = Markdown } in
    Lsif.hover_at context ~filepath:filepath_relative_root ~line ~column
  with
  | exn ->
    Format.eprintf
      "WARNING: semantic lookup for hover info failed (you need to run comby in a git repository \
       for this to work, but maybe you're not doing that): %s@."
    @@ Exn.to_string exn;
    None
