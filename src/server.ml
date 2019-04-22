open Core
open Opium.Std

open Comby
open Language
open Matchers
open Rewriter

let debug = true

let (>>|) = Lwt.Infix.(>|=)

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
  ; in_place_substitutions : Rewrite.match_context_replacement list
  ; id : int
  }
[@@deriving yojson]

let matcher_of_file_extension =
  function
  | ".c" | ".h" | ".cc" | ".cpp" | ".hpp" -> (module Matchers.C : Matchers.Matcher)
  | ".py" -> (module Matchers.Python : Matchers.Matcher)
  | ".go" -> (module Matchers.Go : Matchers.Matcher)
  | ".sh" -> (module Matchers.Bash : Matchers.Matcher)
  | ".html" -> (module Matchers.Html : Matchers.Matcher)
  | ".tex" -> (module Matchers.Html : Matchers.Matcher)
  | _ -> (module Matchers.Generic : Matchers.Matcher)

let matcher_of_language =
  function
  | "c" | "c++" -> (module Matchers.C : Matchers.Matcher)
  | "pyhon" -> (module Matchers.Python : Matchers.Matcher)
  | "go" -> (module Matchers.Go : Matchers.Matcher)
  | "bash" -> (module Matchers.Bash : Matchers.Matcher)
  | "html" -> (module Matchers.Html : Matchers.Matcher)
  | "latex" -> (module Matchers.Latex : Matchers.Matcher)
  | _ -> (module Matchers.Generic : Matchers.Matcher)

let get_matches (module Matcher : Matchers.Matcher) source match_template =
  let configuration = Configuration.create ~match_kind:Fuzzy () in
  Matcher.all ~configuration ~template:match_template ~source

let matches_to_json source id matches =
  Format.sprintf "%s"
    (Yojson.Safe.pretty_to_string (json_match_result_to_yojson { matches; source; id }))

let apply_rule matcher rule =
  let open Option in
  List.filter_map ~f:(fun (Match.{ environment; _ } as matched) ->
      let sat, env = Rule.apply rule ~matcher environment in
      (if sat then env else None)
      >>| fun environment -> { matched with environment })

let perform_match request =
  App.string_of_body_exn request
  >>| Yojson.Safe.from_string
  >>| match_request_of_yojson
  >>| function
  | Ok ({ source; match_template; rule; language; id } as request) ->
    if debug then Format.printf "Received %s@." (Yojson.Safe.pretty_to_string (match_request_to_yojson request));
    let matcher = matcher_of_language language in
    let run ?rule () =
      get_matches matcher source match_template
      |> Option.value_map rule ~default:ident ~f:(apply_rule matcher)
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

let rewrite_to_json id ({ rewritten_source; in_place_substitutions } : Rewrite.result) =
  Format.sprintf "%s"
    (Yojson.Safe.pretty_to_string
       (json_rewrite_result_to_yojson
          { rewritten_source
          ; in_place_substitutions
          ; id
          }))

let perform_rewrite request =
  App.string_of_body_exn request
  >>| Yojson.Safe.from_string
  >>| rewrite_request_of_yojson
  >>| function
  | Ok ({ source; match_template; rewrite_template; rule; language; substitution_kind; id } as request) ->
    if debug then Format.printf "Received %s@." (Yojson.Safe.pretty_to_string (rewrite_request_to_yojson request));
    let matcher = matcher_of_language language in
    let source_substitution =
      match substitution_kind with
      | "newline_separated" -> None
      | "in_place" | _ -> Some source
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
      get_matches matcher source match_template
      |> Option.value_map rule ~default:ident ~f:(apply_rule matcher)
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
  App.empty
  |> App.post "/match" perform_match
  |> App.post "/rewrite" perform_rewrite
  |> App.middleware allow_cors
  |> App.run_command
