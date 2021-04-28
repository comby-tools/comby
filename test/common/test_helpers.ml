open Core

open Rewriter

let configuration = Matchers.Configuration.create ~match_kind:Fuzzy ()

let format s =
  let s = String.chop_prefix_exn ~prefix:"\n" s in
  let leading_indentation = Option.value_exn (String.lfindi s ~f:(fun _ c -> not (Char.equal c ' '))) in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"
  |> String.chop_suffix_exn ~suffix:"\n"

let print_matches matches =
  List.map matches ~f:Match.to_yojson
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

let print_only_match matches =
  List.map matches ~f:(fun matched -> `String matched.Match.matched)
  |> (fun matches -> `List matches)
  |> Yojson.Safe.pretty_to_string
  |> print_string

let run ?(configuration = configuration) (module M : Matchers.Matcher.S) source match_template ?rule rewrite_template =
  let rule =
    match rule with
    | Some rule -> Language.Rule.create rule |> Or_error.ok_exn
    | None -> Language.Rule.create "where true" |> Or_error.ok_exn
  in
  M.all ~configuration ~template:match_template ~source ()
  |> List.filter ~f:(fun { Match.environment; _ } -> Language.Rule.(sat @@ apply ~matcher:(module M) rule environment))
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

let run_nested
    (module M : Matchers.Matcher.S)
    ?(configuration = configuration)
    ?rule
    source
    match_template
    () =
  let nested =
    match rule with
    | None -> true
    | Some rule ->
      let options = Language.Rule.create rule |> Or_error.ok_exn |> Language.Rule.options in
      options.nested
  in
  M.all ~configuration ~nested ~template:match_template ~source ()
  |> function
  | [] -> print_string "No matches."
  | matches ->
    let matches = List.map matches ~f:(Match.convert_offset ~fast:true ~source) in
    Format.asprintf "%a" Match.pp (None, matches)
    |> print_string

(** Rule tests *)
let sat ?(env = Match.Environment.create ()) rule =
  let rule = Language.Rule.create rule |> Or_error.ok_exn in
  Format.sprintf "%b" (Language.Rule.(sat @@ apply rule env))

let make_env bindings =
  List.fold bindings
    ~init:(Match.Environment.create ())
    ~f:(fun env (var, value) -> Match.Environment.add env var value)
