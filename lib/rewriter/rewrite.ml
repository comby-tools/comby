open Core

open Match

type match_context_replacement =
  { range : range
  ; replacement_content : string
  ; environment : environment
  }
[@@deriving yojson]

type result =
  { rewritten_source : string
  ; in_place_substitutions : match_context_replacement list
  }
[@@deriving yojson]

let empty_result =
  { rewritten_source = ""
  ; in_place_substitutions = []
  }
[@@deriving yojson]

let substitute_match_contexts (matches: Match.t list) source replacements =
  let rewrite_template, environment =
    List.fold2_exn
      matches replacements
      ~init:(source, Environment.create ())
      ~f:(fun
           (rewrite_template, accumulator_environment)
           ({ environment = _match_environment; _ } as match_)
           { replacement_content; _ } ->
           (* create a hole in the rewrite template based on this match context *)
           let hole_id, rewrite_template = Rewrite_template.of_match_context match_ ~source:rewrite_template in
           (* add this match context replacement to the environment *)
           let accumulator_environment = Environment.add accumulator_environment hole_id replacement_content in
           (* update match context replacements offset *)
           rewrite_template, accumulator_environment)
  in
  let rewritten_source = Rewrite_template.substitute rewrite_template environment |> fst in
  let offsets = Rewrite_template.get_offsets_for_holes rewrite_template (Environment.vars environment) in
  let offsets = Rewrite_template.get_offsets_after_substitution offsets environment in
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

  (*
   store range information for this match_context replacement:
   (a) its offset in the original source
   (b) its replacement context (to calculate the range)
   (c) an environment of values that are updated to reflect their relative offset in the rewrite template
   *)
let substitute_in_rewrite_template rewrite_template ({ environment; _ } : Match.t) =
  let replacement_content, vars_substituted_for = Rewrite_template.substitute rewrite_template environment in
  let offsets = Rewrite_template.get_offsets_for_holes rewrite_template (Environment.vars environment) in
  let offsets = Rewrite_template.get_offsets_after_substitution offsets environment in
  let environment =
    List.fold offsets ~init:(Environment.create ()) ~f:(fun acc (var, relative_offset) ->
        if List.mem vars_substituted_for var ~equal:String.equal then
          let value = Option.value_exn (Environment.lookup environment var) in
          (* FIXME(RVT): Location does not update row/column here *)
          let start_location =
            Location.{ default with offset = relative_offset }
          in
          let end_location =
            let offset = relative_offset + String.length value in
            Location.{ default with offset }
          in
          let range =
            Range.
              { match_start = start_location
              ; match_end = end_location
              }
          in
          Environment.add ~range acc var value
        else
          acc)
  in
  { replacement_content
  ; environment
  ; range =
      { match_start = { Location.default with offset = 0 }
      ; match_end = Location.default
      }
  }

let all ?source ~rewrite_template matches : result option =
  if matches = [] then None else
    match source with
    (* in-place substitution *)
    | Some source ->
      let matches : Match.t list = List.rev matches in
      matches
      |> List.map ~f:(substitute_in_rewrite_template rewrite_template)
      |> substitute_match_contexts matches source
      |> Option.some
    (* no in place substitution, emit result separated by newlines *)
    | None ->
      matches
      |> List.map ~f:(substitute_in_rewrite_template rewrite_template)
      |> List.map ~f:(fun { replacement_content; _ } -> replacement_content)
      |> String.concat ~sep:"\n"
      |> (fun rewritten_source -> { rewritten_source; in_place_substitutions = [] })
      |> Option.some
