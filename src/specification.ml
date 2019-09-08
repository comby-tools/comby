open Core

type match_specification =
  { match_template : string
  ; rule : string
  }
[@@deriving show]

type rewrite_specification =
  { rewrite_template : string
  ; rule : string
  }
[@@deriving show]

type t =
  { match_specification : match_specification
  ; rewrite_specification : rewrite_specification option
  }
[@@deriving show]

let create
    ?rewrite_template
    ?(rule = "where true")
    ~match_template
    () =
  let match_specification = { match_template; rule } in
  let rewrite_specification =
    Option.map rewrite_template ~f:(fun rewrite_template ->
        { rewrite_template; rule})
  in
  { match_specification; rewrite_specification }
