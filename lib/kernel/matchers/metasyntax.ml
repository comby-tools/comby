include Types.Metasyntax

let default_syntax =
  [ Hole (Everything, Delimited (Some ":[", Some "]"))
  ; Hole (Expression, Delimited (Some ":[", Some ":e]"))
  ; Hole (Alphanum, Delimited (Some ":[[", Some "]]"))
  ; Hole (Non_space, Delimited (Some ":[", Some ".]"))
  ; Hole (Line, Delimited (Some ":[", Some "\\n]"))
  ; Hole (Blank, Delimited (Some ":[ ", Some "]"))
  ; Hole
      ( Expression
      , Reserved_identifiers
          [ "α"
          ; "β"
          ; "γ"
          ; "δ"
          ; "ε"
          ; "ζ"
          ; "η"
          ; "θ"
          ; "ι"
          ; "κ"
          ; "λ"
          ; "μ"
          ; "ξ"
          ; "π"
          ; "ρ"
          ; "ς"
          ; "σ"
          ; "τ"
          ; "υ"
          ; "φ"
          ; "χ"
          ; "ψ"
          ; "ω"
          ] )
  ; Hole (Everything, Reserved_identifiers [ "Γ"; "Δ"; "Θ"; "Λ"; "Ξ"; "Π"; "Σ"; "Φ"; "Ψ"; "Ω" ])
  ; Regex (":[", '~', "]")
  ]

let default_identifier = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
let default_aliases = [ { pattern = "..."; match_template = ":[_]"; rule = None } ]

let default_metasyntax =
  { syntax = default_syntax; identifier = default_identifier; aliases = default_aliases }

let create { syntax; identifier; aliases } =
  let module Metasyntax = struct
    let syntax = syntax
    let identifier = identifier
    let aliases = aliases
  end
  in
  (module Metasyntax : Types.Metasyntax.S)

let default = create default_metasyntax

module Default = (val default)

(* In utop: Format.printf "%s@." @@ Matchers.Metasyntax.(json Matchers.Metasyntax.default_metasyntax);; *)
let json metasyntax = Yojson.Safe.pretty_to_string @@ to_yojson metasyntax
