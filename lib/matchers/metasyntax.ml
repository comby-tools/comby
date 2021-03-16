include Types.Metasyntax

let default_syntax =
  [ Hole (Everything, Delimited (Some ":[", Some "]"))
  ; Hole (Expression, Delimited (Some ":[", Some ":e]"))
  ; Hole (Alphanum, Delimited (Some ":[[", Some "]]"))
  ; Hole (Non_space, Delimited (Some ":[", Some ".]"))
  ; Hole (Line, Delimited (Some ":[", Some "\\n]"))
  ; Hole (Blank, Delimited (Some ":[ ", Some "]"))
  ; Regex (":[", '~', "]")
  ]

let default_identifier =
  function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' -> true
  | _ -> false

let default_metasyntax =
  { syntax = default_syntax
  ; identifier = default_identifier
  }

let create { syntax; identifier } =
  let module Metasyntax = struct
    let syntax = syntax
    let identifier = identifier
  end
  in
  (module Metasyntax : Types.Metasyntax.S)

let default = create default_metasyntax

module Default = (val default)
