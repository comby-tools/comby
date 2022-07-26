open Core_kernel
open Comby_semantic

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

let lsif_hover ~name:_ ~filepath ~line ~column =
  String.chop_prefix_if_exists filepath ~prefix:(Sys.getcwd ())
  |> fun filepath_relative_root ->
  if debug then Format.printf "File relative root: %s@." filepath;
  if debug then Format.printf "Querying type at %d::%d@." line column;
  let context =
    Lsif.Context.
      { repository = "github.com/sourcegraph/sourcegraph"
      ; lsif_endpoint = "https://sourcegraph.com/.api/graphql"
      ; formatting = Markdown ("```go", "```") (* Expose. *)
      }
  in
  Lsif.hover_at context ~filepath:filepath_relative_root ~line ~column
