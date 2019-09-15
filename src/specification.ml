open Core

open Language

type match_specification =
  { match_template : string
  ; rule : Rule.t option
  }

type rewrite_specification =
  { rewrite_template : string
  ; rule : Rule.t option
  }

type t =
  { match_specification : match_specification
  ; rewrite_specification : rewrite_specification option
  }

let create ?rewrite_template ?rule ~match_template () =
  let match_specification = { match_template; rule } in
  let rewrite_specification =
    Option.map rewrite_template ~f:(fun rewrite_template ->
        { rewrite_template; rule})
  in
  { match_specification; rewrite_specification }
