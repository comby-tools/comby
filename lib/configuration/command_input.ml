type single_source =
  | Path of string
  | String of string

type t =
  [ `Paths of string list
  | `Zip of string * Camlzip.Zip.entry list
  | `String of string
  ]

let show_input_kind =
  function
  | String _ -> "A long string..."
  | Path path -> Format.sprintf "A path: %s" path
