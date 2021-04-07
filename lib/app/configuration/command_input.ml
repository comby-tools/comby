type single_source =
  | Path of string
  | String of string

type batch_input =
  [ `Paths of string list
  | `Zip of string * Camlzip.Zip.entry list
  ]

type t =
  [ batch_input
  | `String of string
  ]

let show_input_kind =
  function
  | String _ -> "A long string..."
  | Path path -> Format.sprintf "A path: %s" path
