open Core
open Opium

open Comby
open Matchers
open Rewriter
open Server_types

let (>>|) = Lwt.Infix.(>|=)

let debug =
  match Sys.getenv "DEBUG" with
  | None -> false
  | Some _ -> true

let timeout =
  match Sys.getenv "TIMEOUT" with
  | None -> 30 (* seconds *)
  | Some t -> Int.of_string t


let max_request_length =
  match Sys.getenv "MAX_REQUEST_LENGTH" with
  | None -> Int.max_value
  | Some max -> Int.of_string max

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
  Request.to_plain_text request
  >>| check_too_long
  >>| Result.map ~f:(Fn.compose In.match_request_of_yojson Yojson.Safe.from_string)
  >>| Result.join
  >>| function
  | Ok ({ source; match_template; rule; language; id } as request) ->
    if debug then Format.printf "Received %s@." (Yojson.Safe.pretty_to_string (In.match_request_to_yojson request));
    let matcher =
      match Matchers.Alpha.select_with_extension language with
      | Some matcher -> matcher
      | None -> (module Matchers.Alpha.Generic)
    in
    let run ?rule () =
      let configuration = Matchers.Configuration.create ~match_kind:Fuzzy () in
      let specification = Pipeline.Specification.create ~match_template ?rule () in
      let matches =
        Pipeline.process_single_source
          matcher
          ~sequential:true
          configuration
          (String source)
          specification
        |> function
        | Matches (m, _) -> m
        | _ -> []
      in
      Out.Matches.to_string { matches; source; id }
    in
    let code, result =
      match Option.map rule ~f:Rule.create with
      | None -> 200, run ()
      | Some Ok rule -> 200, run ~rule ()
      | Some Error error -> 400, Error.to_string_hum error
    in
    if debug then Format.printf "Result (%d) %s@." code result;
    Response.make ~status:(Status.of_code code) ~body:(Body.of_string result) ()
  | Error error ->
    if debug then Format.printf "Result (400) %s@." error;
    Response.make ~status:(Status.of_code 400) ~body:(Body.of_string error) ()

let perform_rewrite request =
  Request.to_plain_text request
  >>| check_too_long
  >>| Result.map ~f:(Fn.compose In.rewrite_request_of_yojson Yojson.Safe.from_string)
  >>| Result.join
  >>| function
  | Ok ({ source; match_template; rewrite_template; rule; language; substitution_kind; id } as request) ->
    if debug then Format.printf "Received %s@." (Yojson.Safe.pretty_to_string (In.rewrite_request_to_yojson request));
    let matcher =
      match Matchers.Alpha.select_with_extension language with
      | Some matcher -> matcher
      | None -> (module Matchers.Alpha.Generic)
    in
    let source_substitution, substitute_in_place =
      match substitution_kind with
      | "newline_separated" -> None, false
      | "in_place" | _ -> Some source, true
    in
    let default =
      Out.Rewrite.to_string
        { rewritten_source = ""
        ; in_place_substitutions = []
        ; id
        }
    in
    let run ?rule () =
      let configuration = Configuration.create ~match_kind:Fuzzy () in
      let specification = Pipeline.Specification.create ~match_template ?rule () in
      let matches =
        Pipeline.process_single_source
          matcher
          ~substitute_in_place
          ~sequential:true
          configuration
          (String source)
          specification
        |> function
        | Matches (m, _) -> m
        | _ -> []
      in
      Rewrite.all matches ?source:source_substitution ~rewrite_template
      |> Option.value_map ~default ~f:(fun Replacement.{ rewritten_source; in_place_substitutions } ->
          Out.Rewrite.to_string
            { rewritten_source
            ; in_place_substitutions
            ; id
            })
    in
    let code, result =
      match Option.map rule ~f:Rule.create with
      | None -> 200, run ()
      | Some Ok rule -> 200, run ~rule ()
      | Some Error error -> 400, Error.to_string_hum error
    in
    if debug then Format.printf "Result (%d): %s@." code result;
    Response.make ~status:(Status.of_code code) ~body:(Body.of_string result) ()
  | Error error ->
    if debug then Format.printf "Result (400): %s@." error;
    Response.make ~status:(Status.of_code 400) ~body:(Body.of_string error) ()

let perform_environment_substitution request =
  Request.to_plain_text request
  >>| Yojson.Safe.from_string
  >>| In.substitution_request_of_yojson
  >>| function
  | Ok ({ rewrite_template; environment; id } as request) ->
    if debug then Format.printf "Received %s@." (Yojson.Safe.pretty_to_string (In.substitution_request_to_yojson request));
    let code, result =
      200,
      Out.Substitution.to_string
        { result = fst @@ Rewrite_template.substitute rewrite_template environment
        ; id
        }
    in
    if debug then Format.printf "Result (%d) %s@." code result;
    Response.make ~status:(Status.of_code code) ~body:(Body.of_string result) ()
  | Error error ->
    if debug then Format.printf "Result (400) %s@." error;
    Response.make ~status:(Status.of_code 400) ~body:(Body.of_string error) ()

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
  |> App.post "/substitute" perform_environment_substitution
  |> App.run_command
