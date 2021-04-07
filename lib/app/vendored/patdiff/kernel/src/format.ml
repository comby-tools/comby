open! Core_kernel
open! Import

module Color = struct
  module RGB6 : sig
    type t = private
      { r : int
      ; g : int
      ; b : int
      }
    [@@deriving compare, quickcheck, sexp]

    val create_exn : r:int -> g:int -> b:int -> t
  end = struct
    type t =
      { r : int
      ; g : int
      ; b : int
      }
    [@@deriving compare, quickcheck, sexp]

    let create_exn ~r ~g ~b =
      let check x = 0 <= x && x < 6 in
      if not (check r && check g && check b)
      then invalid_arg "RGB6 (r, g, b) -- expected (0 <= r, g, b < 6)";
      { r; g; b }
    ;;
  end

  module Gray24 : sig
    type t = private { level : int } [@@deriving compare, sexp, quickcheck]

    val create_exn : level:int -> t
  end = struct
    type t = { level : int } [@@deriving compare, quickcheck, sexp]

    let create_exn ~level =
      if not (0 <= level && level < 24)
      then invalid_arg "Gray24 level -- expected (0 <= level < 24)";
      { level }
    ;;
  end

  module T = struct
    type t =
      | Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White
      | Default
      | Gray
      | Bright_black
      | Bright_red
      | Bright_green
      | Bright_yellow
      | Bright_blue
      | Bright_magenta
      | Bright_cyan
      | Bright_white
      | RGB6 of RGB6.t
      | Gray24 of Gray24.t
    [@@deriving compare, quickcheck, sexp]
  end

  include T
  include Comparable.Make (T)

  let rgb6_exn (r, g, b) = RGB6 (RGB6.create_exn ~r ~g ~b)
  let gray24_exn level = Gray24 (Gray24.create_exn ~level)
end

module Style = struct
  module T = struct
    type t =
      | Bold
      | Underline
      | Emph
      | Blink
      | Dim
      | Inverse
      | Hide
      | Reset
      | Foreground of Color.t
      | Fg of Color.t
      | Background of Color.t
      | Bg of Color.t
    [@@deriving compare, quickcheck, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Rule = struct
  module Affix = struct
    type t =
      { text : string
      ; styles : Style.t list
      }
    [@@deriving compare, fields, sexp_of]

    let create ?(styles = []) text = { text; styles }
    let blank = create ""
    let strip_styles t = { t with styles = [] }
  end

  type t =
    { pre : Affix.t
    ; suf : Affix.t
    ; styles : Style.t list
    }
  [@@deriving compare, fields, sexp_of]

  let create ?(pre = Affix.blank) ?(suf = Affix.blank) styles = { pre; suf; styles }
  let blank = create []
  let unstyled_prefix text = { blank with pre = Affix.create text }

  let strip_styles t =
    let f f field = f (Field.get field t) in
    Fields.map
      ~pre:(f Affix.strip_styles)
      ~suf:(f Affix.strip_styles)
      ~styles:(f (const []))
  ;;
end

module Rules = struct
  type t =
    { line_same : Rule.t
    ; line_prev : Rule.t
    ; line_next : Rule.t
    ; line_unified : Rule.t
    ; word_same_prev : Rule.t
    ; word_same_next : Rule.t
    ; word_same_unified : Rule.t
    ; word_prev : Rule.t
    ; word_next : Rule.t
    ; hunk : Rule.t
    ; header_prev : Rule.t
    ; header_next : Rule.t
    }
  [@@deriving compare, fields, sexp_of]

  let inner_line_change text color =
    let style = Style.[ Fg color ] in
    let pre = Rule.Affix.create ~styles:Style.[ Bold; Fg color ] text in
    Rule.create ~pre style
  ;;

  let line_unified =
    let pre = Rule.Affix.create ~styles:Style.[ Bold; Fg Color.Yellow ] "!|" in
    Rule.create ~pre []
  ;;

  let word_change color = Rule.create Style.[ Fg color ]

  let default =
    let open Rule in
    { line_same = unstyled_prefix "  "
    ; line_prev = inner_line_change "-|" Color.Red
    ; line_next = inner_line_change "+|" Color.Green
    ; line_unified
    ; word_same_prev = blank
    ; word_same_next = blank
    ; word_same_unified = blank
    ; word_prev = word_change Color.Red
    ; word_next = word_change Color.Green
    ; hunk = blank
    ; header_prev = blank
    ; header_next = blank
    }
  ;;

  let strip_styles t =
    let f field = Rule.strip_styles (Field.get field t) in
    Fields.map
      ~line_same:f
      ~line_prev:f
      ~line_next:f
      ~line_unified:f
      ~word_same_prev:f
      ~word_same_next:f
      ~word_same_unified:f
      ~word_prev:f
      ~word_next:f
      ~hunk:f
      ~header_prev:f
      ~header_next:f
  ;;
end

module Location_style = struct
  type t =
    | Diff
    | Omake
    | None
  [@@deriving bin_io, compare, quickcheck, enumerate, equal, sexp]

  let to_string = function
    | Diff -> "diff"
    | Omake -> "omake"
    | None -> "none"
  ;;

  let of_string = function
    | "diff" -> Diff
    | "omake" -> Omake
    | "none" -> None
    | other -> failwiths ~here:[%here] "invalid location style" other [%sexp_of: string]
  ;;

  let omake_style_error_message_start ~file ~line =
    sprintf "File \"%s\", line %d, characters 0-1:" file line
  ;;

  let sprint t (hunk : string Patience_diff.Hunk.t) ~prev_filename ~rule =
    match t with
    | Diff ->
      rule
        (sprintf
           "-%i,%i +%i,%i"
           hunk.prev_start
           hunk.prev_size
           hunk.next_start
           hunk.next_size)
    (* omake locations must be parseable, so we can't let the user config insert
       arbitrary prefixes and suffixes and ANSI color rubbish. *)
    | Omake ->
      (* Print line number of first difference, skipping past context lines. *)
      let prev_start =
        with_return (fun r ->
          List.fold hunk.ranges ~init:hunk.prev_start ~f:(fun init ->
            function
            | Same s -> init + Array.length s
            | Prev _ | Next _ | Replace _ | Unified _ -> r.return init))
      in
      omake_style_error_message_start ~file:prev_filename ~line:prev_start
    | None -> rule ""
  ;;
end
