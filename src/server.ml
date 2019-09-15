open Core
open Opium.Std

open Comby
open Language
open Matchers
open Rewriter

let (>>|) = Lwt.Infix.(>|=)

let debug =
  match Sys.getenv "DEBUG" with
  | None -> false
  | Some _ -> true

let max_request_length =
  match Sys.getenv "MAX_REQUEST_LENGTH" with
  | None -> Int.max_value
  | Some max -> Int.of_string max

type match_request =
  { source : string
  ; match_template : string [@key "match"]
  ; rule : string option [@default None]
  ; language : string [@default "generic"]
  ; id : int
  }
[@@deriving yojson]

type rewrite_request =
  { source : string
  ; match_template : string [@key "match"]
  ; rewrite_template : string [@key "rewrite"]
  ; rule : string option [@default None]
  ; language : string [@default "generic"]
  ; substitution_kind : string [@default "in_place"]
  ; id : int
  }
[@@deriving yojson]

type json_match_result =
  { matches : Match.t list
  ; source : string
  ; id : int
  }
[@@deriving yojson]

type json_rewrite_result =
  { rewritten_source : string
  ; in_place_substitutions : Replacement.t list
  ; id : int
  }
[@@deriving yojson]

let matches_to_json source id matches =
  Format.sprintf "%s"
    (Yojson.Safe.pretty_to_string (json_match_result_to_yojson { matches; source; id }))

let check_too_long s =
  let n = String.length s in
  if n > max_request_length then
    Error
      (Format.sprintf
         "The source input is a bit big! Make it %d characters shorter, \
          or click 'Run in Terminal' below to install and run comby locally :)"
         (n - max_request_length))
  else
    Ok s

let perform_match request =
  App.string_of_body_exn request
  >>| (fun s ->
      match check_too_long s with
      | Ok s ->
        Yojson.Safe.from_string s
        |> match_request_of_yojson
      | Error e -> Error e
    )
  >>| function
  | Ok ({ source; match_template; rule; language; id } as request) ->
    if debug then Format.printf "Received %s@." (Yojson.Safe.pretty_to_string (match_request_to_yojson request));
    let matcher = Matchers.select_with_extension language in
    let run ?rule () =
      let configuration = Configuration.create ~match_kind:Fuzzy () in
      Pipeline.run matcher ?rule configuration match_template source
      |> matches_to_json source id
    in
    let code, result =
      match Option.map rule ~f:Rule.create with
      | None -> 200, run ()
      | Some Ok rule -> 200, run ~rule ()
      | Some Error error -> 400, Error.to_string_hum error
    in
    if debug then Format.printf "Result (%d) %s@." code result;
    respond ~code:(`Code code) (`String result)
  | Error error ->
    if debug then Format.printf "Result (400) %s@." error;
    respond ~code:(`Code 400) (`String error)

let rewrite_to_json id ({ rewritten_source; in_place_substitutions } : Replacement.result) =
  Format.sprintf "%s"
    (Yojson.Safe.pretty_to_string
       (json_rewrite_result_to_yojson
          { rewritten_source
          ; in_place_substitutions
          ; id
          }))

let perform_rewrite request =
  App.string_of_body_exn request
  >>| (fun s ->
      match check_too_long s with
      | Ok s ->
        Yojson.Safe.from_string s
        |> rewrite_request_of_yojson
      | Error e -> Error e
    )
  >>| function
  | Ok ({ source; match_template; rewrite_template; rule; language; substitution_kind; id } as request) ->
    if debug then Format.printf "Received %s@." (Yojson.Safe.pretty_to_string (rewrite_request_to_yojson request));
    let matcher = Matchers.select_with_extension language in
    let source_substitution, substitute_in_place =
      match substitution_kind with
      | "newline_separated" -> None, false
      | "in_place" | _ -> Some source, true
    in
    let default =
      { rewritten_source = ""
      ; in_place_substitutions = []
      ; id
      }
      |> json_rewrite_result_to_yojson
      |> Yojson.Safe.pretty_to_string
    in
    let run ?rule () =
      let configuration = Configuration.create ~match_kind:Fuzzy () in
      Pipeline.run matcher ?rule ~newline_separated:(not substitute_in_place) configuration match_template source
      |> Rewrite.all ?source:source_substitution ~rewrite_template
      |> Option.value_map ~default ~f:(rewrite_to_json id)
    in
    let code, result =
      match Option.map rule ~f:Rule.create with
      | None -> 200, run ()
      | Some Ok rule -> 200, run ~rule ()
      | Some Error error -> 400, Error.to_string_hum error
    in
    if debug then Format.printf "Result (%d): %s@." code result;
    respond ~code:(`Code code) (`String result)
  | Error error ->
    if debug then Format.printf "Result (400): %s@." error;
    respond ~code:(`Code 400) (`String error)

let add_cors_headers (headers: Cohttp.Header.t): Cohttp.Header.t =
  Cohttp.Header.add_list headers [
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Allow-Methods", "GET, POST, PATCH, PUT, DELETE, OPTIONS");
    ("Access-Control-Allow-Headers", "Origin, Content-Type, X-Auth-Token");
  ]

let allow_cors =
  let filter handler req =
    handler req
    >>| fun response ->
    response
    |> Response.headers
    |> add_cors_headers
    |> Field.fset Response.Fields.headers response
  in
  Rock.Middleware.create ~name:"allow cors" ~filter

let () =
  Lwt.async_exception_hook := (function
      | Unix.Unix_error (error, func, arg) ->
        Logs.warn (fun m ->
            m "Client connection error %s: %s(%S)"
              (Unix.error_message error) func arg
          )
      | exn -> Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
    );
  App.empty
  |> App.post "/match" perform_match
  |> App.post "/rewrite" perform_rewrite
  |> App.middleware allow_cors
  |> App.run_command
