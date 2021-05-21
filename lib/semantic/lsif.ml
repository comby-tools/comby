open Core_kernel
open Lwt
open Cohttp
open Cohttp_lwt_unix

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

module Formatting = struct
  type t =
    | Markdown of string * string
    | Text

  let hover format text =
    match format with
    | Text -> text
    | Markdown (start, stop) ->
      let lines = text |> String.split_lines |> List.rev in
      let rec aux acc collect = function
        | [] -> acc
        | hd::_ when String.is_prefix hd ~prefix:start ->
          acc
        | hd::tl when String.is_prefix hd ~prefix:stop ->
          aux acc true tl
        | hd::tl when collect ->
          aux (hd::acc) collect tl
        | _::tl ->
          aux acc collect tl
      in
      aux [] false lines |> String.concat ~sep:"\n"
end

module Context = struct
  type t =
    { lsif_endpoint : string
    ; repository : string
    ; formatting : Formatting.t
    }
end

let body Context.{ repository; lsif_endpoint; _ } filepath line character =
  let query = {|{"query":"query Hover($repository: String!, $commit: String!, $path: String!, $line: Int!, $character: Int!) {\n  repository(name: $repository) {\n    commit(rev: $commit) {\n      blob(path: $path) {\n        lsif {\n          hover(line: $line, character: $character) {\n            markdown {\n              text\n            }\n            range {\n              start {\n                line\n                character\n              }\n              end {\n                line\n                character\n              }\n            }\n          }\n        }\n      }\n    }\n  }\n}"|} in
  let variables =
    Format.sprintf
      {|"variables":{"line":%d,"character":%d,"commit":"HEAD","path":"%s","repository":"%s"},"operationName":"Hover"}|}
      line
      character
      filepath
      repository
  in
  let request = Format.sprintf {|%s,%s|} query variables in
  Lwt_unix.sleep 0.25 >>= fun _ ->
  Client.post ~body:(Cohttp_lwt.Body.of_string request) (Uri.of_string lsif_endpoint) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  if debug then Printf.printf "Response code: %d\n" code;
  body |> Cohttp_lwt.Body.to_string

(** {"data":{"repository":{"commit":{"blob":{"lsif":{"hover":{"markdown":{"text":"```go\nvar tr *Trace\n```"},"range":{"start":{"line":64,"character":1},"end":{"line":64,"character":3}}}}}}}}} *)
let hover_at context ~filepath ~line ~column =
  let body =
    Lwt_main.run (body context filepath line column) in
  try
    let response = Yojson.Safe.from_string body in
    if debug then Format.printf "Response: %s@." @@ Yojson.Safe.pretty_to_string response;
    let text =
      response
      |> Yojson.Safe.to_basic
      |> Yojson.Basic.Util.member "data"
      |> Yojson.Basic.Util.member "repository"
      |> Yojson.Basic.Util.member "commit"
      |> Yojson.Basic.Util.member "blob"
      |> Yojson.Basic.Util.member "lsif"
      |> Yojson.Basic.Util.member "hover"
      |> Yojson.Basic.Util.member "markdown"
      |> Yojson.Basic.Util.member "text"
      |> Yojson.Basic.Util.to_string
      |> Formatting.hover context.formatting
    in
    Some text
  with _ -> None
