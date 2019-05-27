type single_input_kind =
  [ `String of string
  | `Path of string
  ]

type t =
  [ `Paths of string list
  | `Zip of string
  | single_input_kind
  ]

let show_input_kind =
  function
  | `Paths _ -> Format.sprintf "Paths..."
  | `Path path -> Format.sprintf "Path: %s" path
  | `String s -> Format.sprintf "String: %s" s
  | `Zip _ -> Format.sprintf "Zip..."
