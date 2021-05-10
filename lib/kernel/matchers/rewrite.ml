open Core_kernel

open Match
open Replacement

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

(* override default metasyntax for identifiers to accomodate fresh variable generation and UUID
   identifiers that contain -, etc. *)
let match_context_syntax =
  let identifier = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-" in
  Metasyntax.{ default_metasyntax with identifier }

let match_context_metasyntax =
  Metasyntax.(create match_context_syntax)

module Match_context_metasyntax = (val match_context_metasyntax)
module Match_context_template = Template.Make(Match_context_metasyntax)

(** Unused alternative to above. Uses String.substr_index on pattern and then String.replace_all. Don't know if it's faster, must benchmark *)
let substitute_in_rewrite_template'
    ?fresh
    ?(metasyntax = Metasyntax.default_metasyntax)
    template
    ({ environment; _ } : Match.t) =
  let (module M) = Metasyntax.create metasyntax in
  let module Template_parser = Template.Make(M) in
  let template = Rewrite_template.substitute_fresh ~metasyntax ?fresh template in
  let vars = Template_parser.variables template in
  let replacement_content, environment =
    List.fold vars ~init:(template, Environment.create ()) ~f:(fun (template, env) { variable; pattern; _ } ->
        match Environment.lookup environment variable with
        | None ->
          template, env
        | Some value ->
          match String.substr_index template ~pattern with
          | Some offset ->
            let range =
              Range.
                { match_start = Location.{ default with offset }
                ; match_end = Location.{ default with offset = offset + String.length value }
                }
            in
            let env = Environment.add ~range env variable value in
            String.substr_replace_all template ~pattern ~with_:value, env
          | None -> template, env)
  in
  { replacement_content
  ; environment
  ; range =
      { match_start = { Location.default with offset = 0 }
      ; match_end = Location.default
      }
  }

let substitute_in_rewrite_template
    ?fresh
    ?(metasyntax = Metasyntax.default_metasyntax)
    template
    environment =
  let (module M) = Metasyntax.create metasyntax in
  let module Template_parser = Template.Make(M) in
  let template = Rewrite_template.substitute_fresh ~metasyntax ?fresh template in
  let terms = Template_parser.parse template in
  let replacement_content, environment, _ =
    List.fold terms ~init:([], Environment.create (), 0) ~f:(fun (result, env, pos) -> function
        | Constant c -> c::result, env, pos + String.length c
        | Hole { variable; pattern; _ } ->
          match Environment.lookup environment variable with
          | None -> pattern::result, env, pos + String.length variable
          | Some value ->
            let advance = pos + String.length value in
            let range =
              Range.
                { match_start = Location.{ default with offset = pos }
                ; match_end = Location.{ default with offset = advance }
                }
            in
            let env = Environment.add ~range env variable value in
            value::result, env, advance)
  in
  let replacement_content = String.concat (List.rev replacement_content) in
  { replacement_content
  ; environment
  ; range =
      { match_start = { Location.default with offset = 0 }
      ; match_end = Location.default
      }
  }

let substitute_match_contexts ?fresh (matches: Match.t list) source replacements =
  if debug then Format.printf "Matches: %d | Replacements: %d@." (List.length matches) (List.length replacements);
  if debug then Option.is_some fresh |> ignore;
  let rewritten_source, in_place_substitutions, _ =
    (* shift adjusts the difference of the matched part and the replacement part to the matched offsets *)
    List.fold2_exn (List.rev matches) (List.rev replacements) ~init:(source, [], 0) ~f:(fun (rolling_result, replacements, shift) ({ range; matched; _ } as m) ({ replacement_content; _ } as r) ->
        if debug then Format.printf "Match: %a" Match.pp (None, [m]);
        if debug then Format.printf "Replace: %s@." @@ (Yojson.Safe.to_string @@ Replacement.to_yojson r);
        if debug then Format.printf "Rolling: %s@." rolling_result;
        if debug then Format.printf "Shift: %d@." shift;
        let start_index = range.match_start.offset + shift in
        let end_index = range.match_end.offset + shift in
        let before = if start_index = 0 then "" else String.slice rolling_result 0 start_index in
        let after = String.slice rolling_result end_index (String.length rolling_result) in
        let match_length = String.length matched (*end_index - start_index*) in
        let difference = String.length replacement_content - match_length in
        if debug then Format.printf "Match len: %d Rep len: %d difference: %d@." match_length (String.length replacement_content) difference;
        if debug then Format.printf "    Start: %d     End: %d@." start_index end_index;
        if debug then Format.printf "NEW Start: %d NEW End: %d@." (start_index + difference) (end_index + difference);
        let range = Range.{ match_start = Location.{ default with offset = start_index }; match_end = Location.{ default with offset = end_index + difference } } in
        let replacements = { r with range }::replacements in
        String.concat [before; replacement_content; after], replacements, shift + difference)
  in
  { rewritten_source
  ; in_place_substitutions
  }

(*
  let rewrite_template, environment =
    List.fold2_exn
      matches replacements
      ~init:(source, Environment.create ())
      ~f:(fun (rewrite_template, accumulator_environment)
           ({ environment = _match_environment; _ } as match_)
           { replacement_content; _ } ->
           (* create a hole in the rewrite template based on this match context *)
           let sub_fresh = Option.map fresh ~f:(fun f -> fun () -> ("sub_" ^ f ())) in (* ensure custom fresh function is unique for substition. *)
           let hole_id, rewrite_template = Rewrite_template.of_match_context ?fresh:sub_fresh match_ ~source:rewrite_template in
           if debug then Format.printf "Created rewrite template with hole var %s: %s @." hole_id rewrite_template;
           (* add this match context replacement to the environment *)
           let accumulator_environment = Environment.add accumulator_environment hole_id replacement_content in
           (* update match context replacements offset *)
           rewrite_template, accumulator_environment)
  in
  if debug then Format.printf "Env:@.%s" (Environment.to_string environment);
  if debug then Format.printf "Rewrite in:@.%s@." rewrite_template;
  let { replacement_content; environment = env; _ } = substitute_in_rewrite_template ~metasyntax:match_context_syntax ?fresh rewrite_template environment in (* replace Rewrite_template.substitute *)
  let terms = Match_context_template.parse rewrite_template in
  let _, env, _ =
    (* use replacements with terms to update offset -- or, just compute it directly -- iteratate over match template and substitute*)
    List.fold terms ~init:([], Environment.create (), 0) ~f:(fun (result, env, pos) -> function
        | Constant c -> c::result, env, pos + String.length c
        | Hole { variable; _ } ->
          match Environment.lookup environment variable with
          | None -> failwith "impossible"
          | Some value ->
            let advance = pos + String.length value in
            let range =
              Range.
                { match_start = Location.{ default with offset = pos }
                ; match_end = Location.{ default with offset = advance }
                }
            in
            let env = Environment.add env ~range variable value in
            value::result, env, advance
      )
  in
  { rewritten_source = replacement_content
  ; in_place_substitutions
  }
*)

let all ?source ?metasyntax ?fresh ~rewrite_template matches : result option =
  if List.is_empty matches then None else
    match source with
    (* in-place substitution *)
    | Some source ->
      let matches : Match.t list = List.rev matches in
      matches
      |> List.map ~f:(fun Match.{ environment; _ } -> substitute_in_rewrite_template ?metasyntax ?fresh rewrite_template environment)
      |> substitute_match_contexts ?fresh matches source
      |> Option.some
    (* no in place substitution, emit result separated by newlines *)
    | None ->
      matches
      |> List.map ~f:(fun Match.{ environment; _ } -> substitute_in_rewrite_template ?metasyntax ?fresh rewrite_template environment)
      |> List.map ~f:(fun { replacement_content; _ } -> replacement_content)
      |> String.concat ~sep:"\n"
      |> (fun rewritten_source -> { rewritten_source; in_place_substitutions = [] })
      |> Option.some
