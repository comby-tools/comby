open! Core_kernel
open! Import
include Html_output_intf

module Make (Mtime : Mtime) = struct
  let string_of_color : Format.Color.t -> string = function
    | Black -> "#000000"
    | Red -> "#880000"
    | Green -> "#008800"
    | Yellow -> "#888800"
    | Blue -> "#000088"
    | Magenta -> "#880088"
    | Cyan -> "#008888"
    | White | Default -> "#ffffff"
    | Gray -> "#c0c0c0"
    | Bright_black -> "#c0c0c0"
    | Bright_red -> "#FF0000"
    | Bright_green -> "#00FF00"
    | Bright_yellow -> "#FFFF00"
    | Bright_blue -> "#0000FF"
    | Bright_magenta -> "#FF00FF"
    | Bright_cyan -> "#00FFFF"
    | Bright_white -> "#FFFFFF"
    | RGB6 { r; g; b } ->
      let percent x = float (x * 100) /. 5.0 in
      sprintf "rgb(%f%%,%f%%,%f%%)" (percent r) (percent g) (percent b)
    | Gray24 { level } ->
      let percent = float (level * 100) /. 23.0 in
      sprintf "rgb(%f%%,%f%%,%f%%)" percent percent percent
  ;;

  module Style = struct
    let apply text ~styles =
      let start_tags, end_tags =
        List.fold styles ~init:([], []) ~f:(fun (s, e) style ->
          match (style : Format.Style.t) with
          | Bold -> "<span style=\"font-weight:bold\">" :: s, "</span>" :: e
          | Reset -> s, e
          | Foreground c | Fg c ->
            ( sprintf "<span style=\"color:%s\">" (string_of_color c) :: s
            , "</span>" :: e )
          | Background c | Bg c ->
            ( sprintf "<span style=\"background-color:%s\">" (string_of_color c) :: s
            , "</span>" :: e )
          | Underline | Emph -> "<u>" :: s, "</u>" :: e
          | Blink -> "<span style=\"text-decoration:blink\">" :: s, "</span>" :: e
          | Inverse -> s, e
          | Hide -> "<!-- " :: s, " -->" :: e
          | Dim ->
            (* "<span style=\"font-weight:lighter\">"::s, "</span>"::e *)
            ( sprintf "<span style=\"color:%s\">" (string_of_color Gray) :: s
            , "</span>" :: e ))
      in
      let lst = start_tags @ [ text ] @ end_tags in
      String.concat ~sep:"" lst
    ;;
  end

  (* assuming we only insert text in contents and not in attributes, only escaping these
     three characters should be enough. We may want to print differently non printable
     ascii characters too? *)
  let html_escape_char = function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | c -> String.of_char c
  ;;

  let html_escape s = String.concat_map s ~f:html_escape_char

  module Rule = struct
    let apply text ~(rule : Format.Rule.t) ~refined =
      let apply styles text = Style.apply text ~styles in
      sprintf
        "%s%s%s"
        (apply rule.pre.styles rule.pre.text)
        (if refined
         then apply [ Format.Style.Reset ] text
         else apply rule.styles (html_escape text))
        (apply rule.suf.styles rule.suf.text)
    ;;
  end

  let print_header ~(rules : Format.Rules.t) ~file_names:(prev_file, next_file) ~print =
    let print_line file rule =
      let get_time file =
        match Mtime.mtime file with
        | Ok time -> Time.to_string time
        | Error _ -> ""
      in
      let time = get_time file in
      print (Rule.apply (sprintf !"%{File_name#hum} %s" file time) ~rule ~refined:false)
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
    print "<pre style=\"font-family:consolas,monospace\">";
    if print_global_header then print_header ~rules ~file_names ~print;
    let f hunk =
      Format.Location_style.sprint
        location_style
        hunk
        ~prev_filename:(File_name.display_name prev_file)
        ~rule:(Rule.apply ~rule:rules.hunk ~refined:false)
      |> print;
      let handle_range : string Patience_diff.Range.t -> unit = function
        (* Just print the new array elements *)
        | Same r ->
          let mr = Array.map r ~f:snd in
          Array.iter mr ~f:print
        | Prev r | Next r | Unified r -> Array.iter r ~f:print
        | Replace (ar1, ar2) ->
          Array.iter ar1 ~f:print;
          Array.iter ar2 ~f:print
      in
      List.iter hunk.ranges ~f:handle_range
    in
    List.iter hunks ~f;
    print "</pre>"
  ;;
end

module Without_mtime = Make (struct
    let mtime _ = Or_error.error_string "Mtime implementation not available"
  end)

module Private = struct
  module Make = Make
end
