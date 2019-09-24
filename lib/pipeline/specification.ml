open Language

type t =
  { match_template : string
  ; rule : Rule.t option
  ; rewrite_template : string option
  }

let create ?rewrite_template ?rule ~match_template () =
  { match_template; rule; rewrite_template }
