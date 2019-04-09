open Core

type match_specification =
  { match_template : string
  ; match_rule : string
  }
[@@deriving show]

type rewrite_specification =
  { rewrite_template : string
  ; rewrite_rule : string
  }
[@@deriving show]

type t =
  { match_specification : match_specification
  ; rewrite_specification : rewrite_specification option
  }
[@@deriving show]

let create
    ?rewrite_template
    ?(match_rule = "where true")
    ?(rewrite_rule = "")
    ~match_template
    () =
  let match_specification = { match_template; match_rule } in
  let rewrite_specification =
    Option.map rewrite_template ~f:(fun rewrite_template ->
        { rewrite_template; rewrite_rule})
  in
  { match_specification; rewrite_specification }
