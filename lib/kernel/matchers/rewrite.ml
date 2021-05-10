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

let substitute_match_contexts ?fresh (matches: Match.t list) source replacements =
  if debug then Format.printf "Matches: %d | Replacements: %d@." (List.length matches) (List.length replacements);
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
  let variables = Match_context_template.variables rewrite_template in
  let rewritten_source = Rewrite_template.substitute ~metasyntax:match_context_syntax ?fresh rewrite_template environment |> fst in
  if debug then Format.printf "Rewritten source:@.%s@." rewritten_source;
  let offsets = Rewrite_template.get_offsets_for_holes variables rewrite_template in
  if debug then
    Format.printf "Replacements: %d | Offsets 1: %d@." (List.length replacements) (List.length offsets);
  let offsets = Rewrite_template.get_offsets_after_substitution offsets environment in
  if debug then
    Format.printf "Replacements: %d | Offsets 2: %d@." (List.length replacements) (List.length offsets);
  let in_place_substitutions =
    List.map2_exn replacements offsets ~f:(fun replacement (_uid, offset) ->
        let match_start = { Location.default with offset } in
        let offset = offset + String.length replacement.replacement_content in
        let match_end = { Location.default with offset } in
        let range = Range.{ match_start; match_end } in
        { replacement with range })
  in
  { rewritten_source
  ; in_place_substitutions
  }

(**
   store range information for this match_context replacement:
   (a) its offset in the original source
   (b) its replacement context (to calculate the range)
   (c) an environment of values that are updated to reflect their relative offset in the rewrite template
*)
let substitute_in_rewrite_template
    ?fresh
    ?(metasyntax = Metasyntax.default_metasyntax)
    template
    ({ environment; _ } : Match.t) =
  let (module M) = Metasyntax.create metasyntax in
  let module Template_parser = Template.Make(M) in
  let template = Rewrite_template.substitute_fresh ~metasyntax ?fresh template in
  let terms = Template_parser.parse template in
  let replacement_content, _, environment =
    List.fold terms ~init:([], 0, Environment.create()) ~f:(fun (result, pos, env) -> function
        | Constant c ->
          c::result, pos + String.length c, env

        | Hole { variable; pattern; _ } ->
          match Environment.lookup environment variable with
          | None -> pattern::result, pos + String.length variable, env
          | Some value ->
            let start_location = Location.{ default with offset = pos } in
            let end_location = Location.{ default with offset = pos + String.length value } in
            let range =
              Range.
                { match_start = start_location
                ; match_end = end_location
                }
            in
            let env = Environment.add ~range env variable value in
            value::result, pos + String.length value, env)
  in
  let replacement_content = String.concat (List.rev replacement_content) in

  { replacement_content
  ; environment
  ; range =
      { match_start = { Location.default with offset = 0 }
      ; match_end = Location.default
      }
  }

let all ?source ?metasyntax ?fresh ~rewrite_template matches : result option =
  if List.is_empty matches then None else
    match source with
    (* in-place substitution *)
    | Some source ->
      let matches : Match.t list = List.rev matches in
      matches
      |> List.map ~f:(substitute_in_rewrite_template ?metasyntax ?fresh rewrite_template)
      |> substitute_match_contexts ?fresh matches source
      |> Option.some
    (* no in place substitution, emit result separated by newlines *)
    | None ->
      matches
      |> List.map ~f:(substitute_in_rewrite_template ?metasyntax ?fresh rewrite_template)
      |> List.map ~f:(fun { replacement_content; _ } -> replacement_content)
      |> String.concat ~sep:"\n"
      |> (fun rewritten_source -> { rewritten_source; in_place_substitutions = [] })
      |> Option.some
