open Core_kernel

open Match

type t =
  { range : range
  ; replacement_content : string
  ; environment : environment
  }
[@@deriving yojson]

type result =
  { rewritten_source : string
  ; in_place_substitutions : t list
  }
[@@deriving yojson]

let empty_result =
  { rewritten_source = ""
  ; in_place_substitutions = []
  }
[@@deriving yojson]

let to_json ?path ?replacements ?rewritten_source ~diff () =
  let uri =
    match path with
    | Some path -> `String path
    | None -> `Null
  in
  match replacements, rewritten_source with
  | Some replacements, Some rewritten_source ->
    `Assoc
      [ "uri", uri
      ; "rewritten_source", `String rewritten_source
      ; "in_place_substitutions", `List (List.map ~f:to_yojson replacements)
      ; "diff", `String diff
      ]
  | _ ->
    `Assoc
      [ "uri", uri
      ; "diff", `String diff
      ]
