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
    | Markdown
    | Text

  let hover format text =
    match format with
    | Text -> text
    | Markdown ->
      let lines = text |> String.split_lines |> List.rev in
      let rec aux acc = function
        | [] -> acc
        | hd :: tl when String.is_prefix hd ~prefix:"```" -> aux acc tl
        | hd :: tl -> aux (hd :: acc) tl
      in
      aux [] lines |> String.concat ~sep:"\n"
end

module Context = struct
  type t =
    { lsif_endpoint : string
    ; repository : string
    ; revision : string
    ; formatting : Formatting.t
    }
end

let body lsif_endpoint request =
  Lwt_unix.sleep 0.25
  >>= fun _ ->
  Client.post ~body:(Cohttp_lwt.Body.of_string request) (Uri.of_string lsif_endpoint)
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  if debug then Format.printf "Response code: %d\n" code;
  body |> Cohttp_lwt.Body.to_string

let gql_repo_indexed_query =
  {|query RepoIndexed($repository: String!) {
  repository(name: $repository) {
    lsifUploads(state: COMPLETED) {
      totalCount
    }
  }
}|}

let repo_indexed = Set_once.create ()

let repo_indexed_check Context.{ repository; lsif_endpoint; _ } =
  let query =
    Format.sprintf
      {|"query": "%s"|}
      (gql_repo_indexed_query |> String.substr_replace_all ~pattern:"\n" ~with_:"\\n")
  in
  let variables = Format.sprintf {|"variables":{"repository":"%s"}|} repository in
  let request = Format.sprintf {|{%s,%s}|} query variables in
  if debug then
    Format.printf
      "Raw equivalent:\n\tcurl '%s' --data-raw $'%s' --compressed@."
      lsif_endpoint
      (request |> String.substr_replace_all ~pattern:"\\n" ~with_:"\\\\n");
  let body = Lwt_main.run (body lsif_endpoint request) in
  try
    let response = Yojson.Safe.from_string body in
    if debug then Format.printf "Response: %s@." @@ Yojson.Safe.pretty_to_string response;
    let count =
      response
      |> Yojson.Safe.to_basic
      |> Yojson.Basic.Util.member "data"
      |> Yojson.Basic.Util.member "repository"
      |> Yojson.Basic.Util.member "lsifUploads"
      |> Yojson.Basic.Util.member "totalCount"
      |> Yojson.Basic.Util.to_int
    in
    count > 0
  with
  | exn ->
    if debug then Format.eprintf "Semantic lib error: %s@." (Exn.to_string exn);
    false

let gql_path_indexed_query =
  {|query PathIndexed($repository: String!, $revision: String!, $path: String!) {
  repository(name: $repository) {
    commit(rev: $revision) {
      path(path: $path) {
        ...on GitBlob {
          lsif {
            lsifUploads {
              id
            }
          }
        }
        ...on GitTree {
          lsif {
            lsifUploads {
              id
            }
          }
        }
      }
    }
  }
}|}

let path_indexed_check Context.{ repository; lsif_endpoint; revision; _ } ~filepath =
  let query =
    Format.sprintf
      {|"query": "%s"|}
      (gql_path_indexed_query |> String.substr_replace_all ~pattern:"\n" ~with_:"\\n")
  in
  let variables =
    Format.sprintf
      {|"variables":{"repository":"%s", "revision":"%s", "path":"%s"}|}
      repository
      revision
      filepath
  in
  let request = Format.sprintf {|{%s,%s}|} query variables in
  if debug then
    Format.printf
      "Raw equivalent:\n\tcurl '%s' --data-raw $'%s' --compressed@."
      lsif_endpoint
      (request |> String.substr_replace_all ~pattern:"\\n" ~with_:"\\\\n");
  let body = Lwt_main.run (body lsif_endpoint request) in
  try
    let response = Yojson.Safe.from_string body in
    if debug then Format.printf "Response: %s@." @@ Yojson.Safe.pretty_to_string response;
    let uploads =
      response
      |> Yojson.Safe.to_basic
      |> Yojson.Basic.Util.member "data"
      |> Yojson.Basic.Util.member "repository"
      |> Yojson.Basic.Util.member "commit"
      |> Yojson.Basic.Util.member "path"
      |> Yojson.Basic.Util.member "lsif"
      |> Yojson.Basic.Util.member "lsifUploads"
      |> Yojson.Basic.Util.to_list
    in
    List.length uploads > 0
  with
  | exn ->
    if debug then Format.eprintf "Semantic lib error: %s@." (Exn.to_string exn);
    false

let gql_hover_at_query =
  {|query Hover($repository: String!, $revision: String!, $path: String!, $line: Int!, $character: Int!) {
  repository(name: $repository) {
    commit(rev: $revision) {
      blob(path: $path) {
        lsif {
          hover(line: $line, character: $character) {
            markdown {
              text
            }
            range {
              start {
                line
                character
              }
              end {
                line
                character
              }
            }
          }
        }
      }
    }
  }
}|}

let hover_at
    (Context.{ repository; lsif_endpoint; revision; formatting } as context)
    ~filepath
    ~line
    ~column
  =
  let repo_ok =
    match Set_once.get repo_indexed with
    | None ->
      let indexed = repo_indexed_check context in
      Set_once.set_if_none repo_indexed Lexing.dummy_pos indexed;
      indexed
    | Some ok -> ok
  in
  if not repo_ok then (
    Format.eprintf {|❌ no hover data indexed for repo %s@.|} repository;
    None)
  else if not (path_indexed_check context ~filepath) then (
    Format.eprintf {|❌ no hover data indexed for %s@.|} filepath;
    None)
  else (
    if debug then Format.printf "Path %s is indexed@." filepath;
    Format.eprintf {|✅ hover data indexed for %s@.|} filepath;
    let query =
      Format.sprintf
        {|"query": "%s"|}
        (gql_hover_at_query |> Stringext.replace_all ~pattern:"\n" ~with_:"\\n")
    in
    let variables =
      Format.sprintf
        {|"variables":{"line":%d,"character":%d,"revision":"%s","path":"%s","repository":"%s"}|}
        line
        column
        revision
        filepath
        repository
    in
    let request = Format.sprintf {|{%s,%s}|} query variables in
    if debug then
      Format.printf
        "Raw equivalent:\n\tcurl '%s' --data-raw $'%s' --compressed@."
        lsif_endpoint
        (request |> String.substr_replace_all ~pattern:"\\n" ~with_:"\\\\n");
    let body = Lwt_main.run (body lsif_endpoint request) in
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
        |> Formatting.hover formatting
      in
      Some text
    with
    | exn ->
      if debug then Format.eprintf "Semantic lib error: %s@." (Exn.to_string exn);
      None)
