open! Core_kernel
open! Import

(* From https://en.wikipedia.org/wiki/ANSI_escape_code last accessed 2016-05-26 *)

module RGB6 = struct
  include Format.Color.RGB6

  let escape_code { r; g; b } = 16 + (36 * r) + (6 * g) + b
end

module Gray24 = struct
  include Format.Color.Gray24

  let escape_code { level } = 232 + level
end

let codes_of_foreground_color : Format.Color.t -> string = function
  | Black -> "30"
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Blue -> "34"
  | Magenta -> "35"
  | Cyan -> "36"
  | White -> "37"
  | Default -> "39"
  | Gray | Bright_black -> "90"
  | Bright_red -> "91"
  | Bright_green -> "92"
  | Bright_yellow -> "93"
  | Bright_blue -> "94"
  | Bright_magenta -> "95"
  | Bright_cyan -> "96"
  | Bright_white -> "97"
  | RGB6 x -> sprintf "38;5;%d" (RGB6.escape_code x)
  | Gray24 x -> sprintf "38;5;%d" (Gray24.escape_code x)
;;

let codes_of_background_color : Format.Color.t -> string = function
  | Black -> "40"
  | Red -> "41"
  | Green -> "42"
  | Yellow -> "43"
  | Blue -> "44"
  | Magenta -> "45"
  | Cyan -> "46"
  | White -> "47"
  | Default -> "49"
  | Gray | Bright_black -> "100"
  | Bright_red -> "101"
  | Bright_green -> "102"
  | Bright_yellow -> "103"
  | Bright_blue -> "104"
  | Bright_magenta -> "105"
  | Bright_cyan -> "106"
  | Bright_white -> "107"
  | RGB6 x -> sprintf "48;5;%d" (RGB6.escape_code x)
  | Gray24 x -> sprintf "48;5;%d" (Gray24.escape_code x)
;;

let codes_of_style : Format.Style.t -> string = function
  | Reset -> "0"
  | Bold -> "1"
  | Dim -> "2"
  | Underline | Emph -> "4"
  | Blink -> "5"
  | Inverse -> "7"
  | Hide -> "8"
  | Fg c | Foreground c -> codes_of_foreground_color c
  | Bg c | Background c -> codes_of_background_color c
;;

let apply_styles ?(drop_leading_resets = false) (styles : Format.Style.t list) str =
  let styles =
    if drop_leading_resets
    then
      List.drop_while styles ~f:(function
        | Reset -> true
        | _ -> false)
    else (
      match styles with
      | [] -> []
      | Reset :: _ -> styles
      | _ :: _ -> Reset :: styles)
  in
  match styles with
  | [] -> str
  | _ ->
    sprintf
      "\027[%sm%s\027[0m"
      (List.map styles ~f:codes_of_style |> String.concat ~sep:";")
      str
;;

module Rule = struct
  let apply text ~(rule : Format.Rule.t) ~refined =
    let only_whitespace =
      (not (String.is_empty text)) && String.for_all text ~f:Char.is_whitespace
    in
    let text_style : Format.Style.t list =
      if List.is_empty rule.styles
      then []
      else (
        match refined, only_whitespace with
        | true, _ -> [ Reset ]
        | false, true -> Inverse :: rule.styles
        | false, false -> rule.styles)
    in
    sprintf
      "%s%s%s"
      (apply_styles rule.pre.styles rule.pre.text)
      (apply_styles text_style text)
      (apply_styles rule.suf.styles rule.suf.text)
  ;;
end

let print_header ~(rules : Format.Rules.t) ~file_names:(prev_file, next_file) ~print =
  let print_line (file : File_name.t) rule =
    print (Rule.apply (File_name.display_name file) ~rule ~refined:false)
  in
  print_line prev_file rules.header_prev;
  print_line next_file rules.header_next
;;

let print
      ~print_global_header
      ~file_names:((prev_file, _) as file_names)
      ~(rules : Format.Rules.t)
      ~print
      ~location_style
      hunks
  =
  let f_hunk_break hunk =
    Format.Location_style.sprint
      location_style
      hunk
      ~prev_filename:(File_name.display_name prev_file)
      ~rule:(Rule.apply ~rule:rules.hunk ~refined:false)
    |> print
  in
  if print_global_header then print_header ~rules ~file_names ~print;
  Hunks.iter' ~f_hunk_break ~f_line:print hunks
;;
