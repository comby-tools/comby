open! Core
open! Import
include Patdiff_kernel.Configuration

module On_disk = struct
  module Affix = struct
    (* Simply a prefix or a suffix.  Might come with an associated style. *)

    type t =
      { text : string option [@sexp.option]
      ; style : Format.Style.t list option [@sexp.option]
      }
    [@@deriving quickcheck, sexp]

    let blank = { text = None; style = None }
    let get_text t = Option.value t.text ~default:""

    let length t_opt =
      let f t = String.length (get_text t) in
      Option.value_map t_opt ~default:0 ~f
    ;;

    let to_internal t ~min_width =
      let text = sprintf "%*s" min_width (get_text t) in
      let styles = Option.value ~default:[] t.style in
      Format.Rule.Affix.create ~styles text
    ;;
  end

  module Rule = struct
    type t =
      { prefix : Affix.t option [@sexp.option]
      ; suffix : Affix.t option [@sexp.option]
      ; style : Format.Style.t list option [@sexp.option]
      }
    [@@deriving quickcheck, sexp]

    let blank = { prefix = None; suffix = None; style = None }

    let to_internal t =
      let f = Affix.to_internal ~min_width:0 in
      let default = Format.Rule.Affix.blank in
      let affix opt = Option.value_map ~default ~f opt in
      let pre = affix t.prefix in
      let suf = affix t.suffix in
      let style = Option.value ~default:[] t.style in
      Format.Rule.create ~pre ~suf style
    ;;
  end

  module Hunk = struct
    type t = Rule.t [@@deriving quickcheck, sexp]

    let to_internal t =
      let get_affix a = Option.value a ~default:Affix.blank in
      let prefix = get_affix t.Rule.prefix in
      let suffix = get_affix t.Rule.suffix in
      let prefix_text = Option.value prefix.Affix.text ~default:"@@ " in
      let suffix_text = Option.value suffix.Affix.text ~default:" @@" in
      let t =
        { t with
          Rule.prefix = Some { prefix with Affix.text = Some prefix_text }
        ; Rule.suffix = Some { suffix with Affix.text = Some suffix_text }
        }
      in
      Rule.to_internal t
    ;;
  end

  module Header = struct
    type t = Rule.t [@@deriving quickcheck, sexp]

    let to_internal t ~default =
      let get_affix a = Option.value a ~default:Affix.blank in
      let prefix = get_affix t.Rule.prefix in
      let prefix_text = Option.value prefix.Affix.text ~default in
      let t =
        { t with Rule.prefix = Some { prefix with Affix.text = Some prefix_text } }
      in
      Rule.to_internal t
    ;;
  end

  module Line_rule = struct
    type t =
      { prefix : Affix.t option [@sexp.option]
      ; suffix : Affix.t option [@sexp.option]
      ; style : Format.Style.t list option [@sexp.option]
      ; word_same : Format.Style.t list option [@sexp.option]
      }
    [@@deriving sexp, quickcheck]

    let default = { prefix = None; suffix = None; style = None; word_same = None }
  end

  module V1 = struct
    type t =
      { dont_produce_unified_lines : bool option [@sexp.option]
      ; dont_overwrite_word_old_word_new : bool option [@sexp.option]
      ; config_path : string option [@sexp.option]
      ; context : int option [@sexp.option]
      ; line_big_enough : int option [@sexp.option]
      ; word_big_enough : int option [@sexp.option]
      ; unrefined : bool option [@sexp.option]
      ; keep_whitespace : bool option [@sexp.option]
      ; split_long_lines : bool option [@sexp.option]
      ; interleave : bool option [@sexp.option]
      ; assume_text : bool option [@sexp.option]
      ; quiet : bool option [@sexp.option]
      ; shallow : bool option [@sexp.option]
      ; double_check : bool option [@sexp.option]
      ; mask_uniques : bool option [@sexp.option]
      ; html : bool option [@sexp.option]
      ; alt_old : string option [@sexp.option]
      ; alt_new : string option [@sexp.option]
      ; ext_cmp : string option [@sexp.option]
      ; header_old : Header.t option [@sexp.option]
      ; header_new : Header.t option [@sexp.option]
      ; hunk : Hunk.t option [@sexp.option]
      ; line_same : Line_rule.t option [@sexp.option]
      ; line_old : Line_rule.t option [@sexp.option]
      ; line_new : Line_rule.t option [@sexp.option]
      ; line_unified : Line_rule.t option [@sexp.option]
      ; word_old : Rule.t option [@sexp.option]
      ; word_new : Rule.t option [@sexp.option]
      ; location_style : Format.Location_style.t
                         [@default Format.Location_style.Diff] [@sexp_drop_default.equal]
      ; warn_if_no_trailing_newline_in_both : bool
                                              [@default warn_if_no_trailing_newline_in_both_default]
                                              [@sexp_drop_default.equal]
      }
    [@@deriving quickcheck, sexp]
  end

  module V0 = struct
    module Line_changed = struct
      type t =
        { prefix_old : Affix.t
        ; prefix_new : Affix.t
        }
      [@@deriving sexp]
    end

    module Word_same = struct
      type t =
        { style_old : Format.Style.t list
        ; style_new : Format.Style.t list
        }
      [@@deriving sexp]
    end

    module Word_changed = struct
      type t =
        { style_old : Format.Style.t list
        ; style_new : Format.Style.t list
        ; prefix_old : Affix.t option [@sexp.option]
        ; suffix_old : Affix.t option [@sexp.option]
        ; prefix_new : Affix.t option [@sexp.option]
        ; suffix_new : Affix.t option [@sexp.option]
        }
      [@@deriving sexp]
    end

    module Old_header = struct
      type t =
        { style_old : Format.Style.t list option [@sexp.option]
        ; style_new : Format.Style.t list option [@sexp.option]
        ; prefix_old : Affix.t option [@sexp.option]
        ; suffix_old : Affix.t option [@sexp.option]
        ; prefix_new : Affix.t option [@sexp.option]
        ; suffix_new : Affix.t option [@sexp.option]
        }
      [@@deriving sexp]
    end

    type t =
      { config_path : string option [@sexp.option]
      ; context : int option [@sexp.option]
      ; line_big_enough : int option [@sexp.option]
      ; word_big_enough : int option [@sexp.option]
      ; unrefined : bool option [@sexp.option]
      ; external_compare : string option [@sexp.option]
      ; keep_whitespace : bool option [@sexp.option]
      ; split_long_lines : bool option [@sexp.option]
      ; interleave : bool option [@sexp.option]
      ; assume_text : bool option [@sexp.option]
      ; shallow : bool option [@sexp.option]
      ; quiet : bool option [@sexp.option]
      ; double_check : bool option [@sexp.option]
      ; hide_uniques : bool option [@sexp.option]
      ; header : Old_header.t option [@sexp.option]
      ; line_same : Format.Style.t list option [@sexp.option]
      ; line_same_prefix : Affix.t option [@sexp.option]
      ; line_changed : Line_changed.t option [@sexp.option]
      ; word_same : Word_same.t option [@sexp.option]
      ; word_changed : Word_changed.t option [@sexp.option]
      ; chunk : Hunk.t option [@sexp.option]
      ; location_style : Format.Location_style.t
                         [@default Format.Location_style.Diff] [@sexp_drop_default.equal]
      ; warn_if_no_trailing_newline_in_both : bool
                                              [@default warn_if_no_trailing_newline_in_both_default]
                                              [@sexp_drop_default.equal]
      }
    [@@deriving sexp]

    let to_v1 t =
      { V1.config_path = t.config_path
      ; context = t.context
      ; line_big_enough = t.line_big_enough
      ; word_big_enough = t.word_big_enough
      ; unrefined = t.unrefined
      ; dont_produce_unified_lines = None
      ; dont_overwrite_word_old_word_new = None
      ; keep_whitespace = t.keep_whitespace
      ; interleave = t.interleave
      ; assume_text = t.assume_text
      ; split_long_lines = t.split_long_lines
      ; quiet = t.quiet
      ; shallow = t.shallow
      ; double_check = t.double_check
      ; mask_uniques = t.hide_uniques
      ; html = None
      ; alt_old = None
      ; alt_new = None
      ; ext_cmp = t.external_compare
      ; header_old =
          Option.map t.header ~f:(fun header ->
            { Rule.style = header.Old_header.style_old
            ; prefix = header.Old_header.prefix_old
            ; suffix = header.Old_header.suffix_old
            })
      ; header_new =
          Option.map t.header ~f:(fun header ->
            { Rule.style = header.Old_header.style_new
            ; prefix = header.Old_header.prefix_new
            ; suffix = header.Old_header.suffix_new
            })
      ; hunk = t.chunk
      ; line_same =
          Some
            { Line_rule.default with
              Line_rule.style = t.line_same
            ; prefix = t.line_same_prefix
            ; word_same =
                Option.map t.word_same ~f:(fun word_same ->
                  word_same.Word_same.style_old)
            }
      ; line_old =
          Option.map t.line_changed ~f:(fun line_changed ->
            { Line_rule.default with
              Line_rule.style =
                Option.map t.word_changed ~f:(fun word_changed ->
                  word_changed.Word_changed.style_old)
            ; Line_rule.prefix = Some line_changed.Line_changed.prefix_old
            ; word_same =
                Option.map t.word_same ~f:(fun word_same ->
                  word_same.Word_same.style_old)
            })
      ; line_new =
          Option.map t.line_changed ~f:(fun line_changed ->
            { Line_rule.default with
              Line_rule.style =
                Option.map t.word_changed ~f:(fun word_changed ->
                  word_changed.Word_changed.style_new)
            ; Line_rule.prefix = Some line_changed.Line_changed.prefix_new
            ; word_same =
                Option.map t.word_same ~f:(fun word_same ->
                  word_same.Word_same.style_new)
            })
      ; line_unified = None
      ; word_old = None
      ; word_new = None
      ; location_style = t.location_style
      ; warn_if_no_trailing_newline_in_both = t.warn_if_no_trailing_newline_in_both
      }
    ;;
  end

  type t = V1.t [@@deriving sexp_of]

  let t_of_sexp sexp =
    match V1.t_of_sexp sexp with
    | v1 -> v1
    | exception as_v1_exn ->
      (match V0.t_of_sexp sexp with
       | v0 -> V0.to_v1 v0
       | exception as_v0_exn ->
         raise_s
           [%message
             "Patdiff.Configuration.On_disk.t_of_sexp: invalid config"
               (as_v1_exn : exn)
               (as_v0_exn : exn)])
  ;;
end

let parse
      ({ dont_produce_unified_lines
       ; dont_overwrite_word_old_word_new
       ; config_path =
           _
       ; context
       ; line_big_enough
       ; word_big_enough
       ; unrefined
       ; keep_whitespace
       ; split_long_lines
       ; interleave
       ; assume_text
       ; quiet
       ; shallow
       ; double_check
       ; mask_uniques
       ; html
       ; alt_old
       ; alt_new
       ; ext_cmp
       ; header_old
       ; header_new
       ; hunk
       ; line_same
       ; line_old
       ; line_new
       ; line_unified
       ; word_old
       ; word_new
       ; location_style
       ; warn_if_no_trailing_newline_in_both
       } :
         On_disk.t)
  =
  let default_true = Option.value ~default:true in
  let default_false = Option.value ~default:false in
  let default_rule = Option.value ~default:On_disk.Line_rule.default in
  (* Lines *)
  let line_same = default_rule line_same in
  let line_prev = default_rule line_old in
  let line_next = default_rule line_new in
  let line_unified = default_rule line_unified in
  (* Padding for prefixes: They should all be the same length. *)
  let min_width =
    [ line_same; line_prev; line_next; line_unified ]
    |> List.map ~f:(fun line -> On_disk.Affix.length line.prefix)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let create_line (line : On_disk.Line_rule.t) =
    Format.Rule.create
      (Option.value ~default:[] line.style)
      ~pre:
        (On_disk.Affix.to_internal
           ~min_width
           (Option.value ~default:On_disk.Affix.blank line.prefix))
  in
  (* Words *)
  let create_word_same (line : On_disk.Line_rule.t) =
    Format.Rule.create (Option.value ~default:[] line.word_same)
  in
  let create_word ~(line_rule : On_disk.Line_rule.t) opt =
    let rule =
      if default_false dont_overwrite_word_old_word_new
      then Option.value ~default:On_disk.Rule.blank opt
      else { On_disk.Rule.blank with style = line_rule.style }
    in
    On_disk.Rule.to_internal rule
  in
  (* Header *)
  let create_header h_opt prefix =
    On_disk.Header.to_internal
      (Option.value ~default:On_disk.Rule.blank h_opt)
      ~default:prefix
  in
  (* Final *)
  let t =
    create_exn
      ~rules:
        { Format.Rules.line_same = create_line line_same
        ; line_prev = create_line line_prev
        ; line_next = create_line line_next
        ; line_unified = create_line line_unified
        ; word_same_prev = create_word_same line_prev
        ; word_same_next = create_word_same line_next
        ; word_same_unified = create_word_same line_unified
        ; word_prev = create_word ~line_rule:line_prev word_old
        ; word_next = create_word ~line_rule:line_next word_new
        ; hunk = On_disk.Hunk.to_internal (Option.value hunk ~default:On_disk.Rule.blank)
        ; header_prev = create_header header_old "---"
        ; header_next = create_header header_new "+++"
        }
      ~output:(if default_false html then Html else Ansi)
      ~context:(Option.value ~default:(-1) context)
      ~word_big_enough:(Option.value ~default:default_word_big_enough word_big_enough)
      ~line_big_enough:(Option.value ~default:default_line_big_enough line_big_enough)
      ~unrefined:(default_false unrefined)
      ~produce_unified_lines:(not (default_false dont_produce_unified_lines))
      ~float_tolerance:None
      ~keep_ws:(default_false keep_whitespace)
      ~split_long_lines:(default_false split_long_lines)
      ~interleave:(default_true interleave)
      ~assume_text:(default_false assume_text)
      ~shallow:(default_false shallow)
      ~quiet:(default_false quiet)
      ~double_check:(default_false double_check)
      ~mask_uniques:(default_false mask_uniques)
      ~prev_alt:alt_old
      ~next_alt:alt_new
      ~location_style
      ~warn_if_no_trailing_newline_in_both
  in
  match ext_cmp with
  | None -> t
  | Some _ as some ->
    (Private.with_ext_cmp [@alert "-deprecated"]) t ~ext_cmp:some ~notify:ignore
;;

let dark_bg =
  lazy
    (let sexp =
       (* this sexp is copied from /mnt/global/dev/etc/shared/patdiff-dark-bg *)
       Sexp.of_string
         {|
((context 8)
 (line_same ())
 (line_changed
  ((prefix_old ((text "-|") (style (Bold (Fg Red)))))
   (prefix_new ((text "+|") (style (Bold (Fg Green)))))))
 (word_same ((style_old ())
             (style_new ())))
 (word_changed ((style_old (Bold Underline (Fg Red)))
                (style_new ((Fg Green)))))
 (chunk
  ((prefix ((text "@@@@@@@@@@ ") (style (Bold (Fg blue)))))
   (suffix ((text " @@@@@@@@@@") (style (Bold (Fg blue)))))
   (style (Bold (Fg blue)))))
 )|}
     in
     parse (On_disk.V0.t_of_sexp sexp |> On_disk.V0.to_v1))
;;

let light_bg =
  lazy
    (let sexp =
       (* this sexp is copied from /mnt/global/dev/etc/shared/patdiff-light-bg *)
       Sexp.of_string
         {|
((context 8)
 (line_same (dim))
 (line_changed ((prefix_old ((text "-|") (style (bold (fg red)))))
                (prefix_new ((text "+|") (style (bold (fg green)))))))
 (word_same ((style_old ((bg white)))
             (style_new ((bg yellow)))))
 (word_changed ((style_old ((bg white) bold))
                (style_new ((bg yellow) bold))))
 )|}
     in
     parse (On_disk.V0.t_of_sexp sexp |> On_disk.V0.to_v1))
;;

let%test_module _ =
  (module struct
    (* Ensure both sexps are parseable *)
    let%test_unit _ =
      let dark = Lazy.force dark_bg in
      let light = Lazy.force light_bg in
      ignore (dark : t);
      ignore (light : t)
    ;;
  end)
;;

let load_sexp_conv f conv = Result.try_with (fun () -> Sexp.load_sexp_conv_exn f conv)

let rec load_exn' ~set config_file =
  let config =
    match load_sexp_conv config_file On_disk.V1.t_of_sexp with
    | Ok c -> c
    | Error exn ->
      let as_old_config =
        Result.map (load_sexp_conv config_file On_disk.V0.t_of_sexp) ~f:On_disk.V0.to_v1
      in
      (match as_old_config with
       | Error _another_exn -> raise exn
       | Ok c ->
         (let new_file = config_file ^ ".new" in
          match Sys.file_exists new_file with
          | `Yes | `Unknown -> ()
          | `No ->
            (try Sexp.save_hum new_file (On_disk.V1.sexp_of_t c) with
             | _ -> ()));
         c)
  in
  match config.config_path with
  | Some config_path ->
    if Set.mem set config_path
    then failwith "Cycle detected! file redirects to itself"
    else load_exn' ~set:(Set.add set config_path) config_path
  | None -> parse config
;;

let load_exn config_file = load_exn' ~set:String.Set.empty config_file

(* prints errors to stderr *)
let load ?(quiet_errors = false) config_file =
  try Some (load_exn config_file) with
  | e ->
    if not quiet_errors
    then eprintf "Note: error loading %S: %s\n%!" config_file (Exn.to_string e);
    None
;;

let default_string =
  sprintf
    {|;; -*- scheme -*-
;; patdiff Configuration file

(
 (context %d)

 (line_same
  ((prefix ((text " |") (style ((bg bright_black) (fg black)))))))

 (line_old
  ((prefix ((text "-|") (style ((bg red)(fg black)))))
   (style ((fg red)))
   (word_same (dim))))

 (line_new
  ((prefix ((text "+|") (style ((bg green)(fg black)))))
   (style ((fg green)))))

 (line_unified
  ((prefix ((text "!|") (style ((bg yellow)(fg black)))))))

 (header_old
  ((prefix ((text "------ ") (style ((fg red)))))
   (style (bold))))

 (header_new
  ((prefix ((text "++++++ ") (style ((fg green)))))
   (style (bold))))


 (hunk
  ((prefix ((text "@|") (style ((bg bright_black) (fg black)))))
   (suffix ((text " ============================================================") (style ())))
   (style (bold))))
)|}
    default_context
;;

let%test_unit "default Config.t sexp matches default Configuration.t" =
  let default_from_disk =
    parse ([%of_sexp: On_disk.t] (Sexp.of_string default_string))
  in
  [%test_eq: t] default default_from_disk
;;

let get_config ?filename () =
  (* Load config file if it exists, use default if not *)
  let file =
    match filename with
    | Some "" -> None
    | Some f -> Some f (* specified file *)
    | None ->
      (* ~/.patdiff exists *)
      Option.bind (Sys.getenv "HOME") ~f:(fun home ->
        let f = home ^/ ".patdiff" in
        match Sys.file_exists f with
        | `Yes -> Some f
        | `No | `Unknown -> None)
  in
  (* load prints warnings to stderr. This is desired because [file] is only Some if it
     was manually specified or if ~/.patdiff exists. The user should be notified of
     errors if the file fails in both cases. *)
  match Option.bind file ~f:load with
  | Some c -> c
  | None -> default
;;

let save_default ~filename = Out_channel.write_all filename ~data:default_string

include struct
  let%test_unit "default config parses" = ignore (get_config ~filename:"" () : t)
end
