open! Core_kernel
open! Import

type t =
  | Real of
      { real_name : string
      ; alt_name : string option
      }
  | Fake of string
[@@deriving compare, equal]

let real_name_exn = function
  | Real { real_name; alt_name = _ } -> real_name
  | Fake _ -> raise_s [%message "File_name.real_name_exn got a fake file"]
;;

let display_name = function
  | Real { real_name; alt_name } -> Option.value alt_name ~default:real_name
  | Fake name -> name
;;

let to_string_hum = display_name

let append t part =
  match t with
  | Real { real_name; alt_name } ->
    Real
      { real_name = Filename.concat real_name part
      ; alt_name = Option.map alt_name ~f:(fun name -> Filename.concat name part)
      }
  | Fake name -> Fake (Filename.concat name part)
;;

let dev_null = Real { real_name = "/dev/null"; alt_name = None }
