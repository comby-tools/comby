open Core

include Types.Metasyntax

let default_definition =
  [ Delimited (Everything, Some ":[", Some "]")
  ; Delimited (Expression, Some ":[", Some ":e]")
  ; Delimited (Alphanum, Some ":[[", Some "]]")
  ; Delimited (Non_space, Some ":[", Some ".]")
  ; Delimited (Line, Some ":[", Some "\\n]")
  ; Delimited (Blank, Some ":[ ", Some "]")
  ; Regex (":[", '~', "]")
  ]

let default_identifier =
  function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' -> true
  | _ -> false

let default_metasyntax = { definition = default_definition; identifier = default_identifier }

let create { definition; identifier } =
  let open Polymorphic_compare in
  let hole sort =
    List.find_map definition ~f:(function
        | Delimited (sort', Some left, None)
        | Prefix (sort', left) when sort' = sort -> Some (Some left, None)
        | Delimited (sort', left, right) when sort' = sort -> Some (left, right)
        | _ -> None)
  in
  let regex =
    List.find_map definition ~f:(function
        | Regex (left, separator, right) -> Some (left, separator, right)
        | _ -> None)
  in
  let module Metasyntax = struct
    let everything  = hole Everything
    let expression = hole Expression
    let alphanum = hole Alphanum
    let non_space  = hole Non_space
    let line = hole Line
    let blank = hole Blank
    let regex = regex
    let identifier = identifier
  end
  in
  (module Metasyntax : Types.Metasyntax.S)

let default = create default_metasyntax

module Default = (val default)
