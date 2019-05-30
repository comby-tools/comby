open Core
open Patdiff_lib

open Configuration

(* This is the default patdiff configuration. *)
let default context =
  Format.sprintf
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
)|} context

let terminal ?(context = 16) () =
  Config.t_of_sexp (Sexp.of_string (default context))
  |> parse

let diff_configuration =
  {|;; -*- scheme -*-
;; patdiff Configuration file

(
 (context 1)

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
   (suffix ((text " =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=") (style ())))
   (style (bold))))
)|}

let match_diff () =
  Config.t_of_sexp (Sexp.of_string diff_configuration)
  |> parse

(* Needs (unrefined true), otherwise it just prints without colors. Unrefined true
   will diff on a line basis. line_unified is ignored for unrefined, but
   will still create a prefix width of 2 in the diff if it is "!|" *)
let plain_configuration =
  {|;; -*- scheme -*-
;; patdiff Configuration file

(
 (context 3)
 (unrefined true)

 (line_same
  ((prefix ((text " ") (style ())))))

 (line_old
  ((prefix ((text "-") (style ())))
   (style ())
   (word_same (dim))))

 (line_new
  ((prefix ((text "+") (style ())))
   (style ())))

 (header_old
  ((prefix ((text "--- ") (style ())))
   (style ())))

 (header_new
  ((prefix ((text "+++ ") (style ())))
   (style ())))

 (hunk
  ((prefix ((text "@@ ") (style ())))
   (suffix ((text " @@") (style ())))
   (style ())))
)
|}

let plain () =
  Config.t_of_sexp (Sexp.of_string plain_configuration)
  |> parse

type kind =
  | Plain
  | Colored
  | Html
  | Default
  | Match_only

let get_diff kind source_path source_content result =
  let open Patdiff_lib in
  let source_path =
    match source_path with
    | Some path -> path
    | None -> "/dev/null"
  in
  let configuration =
    match kind with
    | Plain -> plain ()
    | Colored
    | Html
    | Default -> terminal ~context:3 ()
    | Match_only -> match_diff ()
  in
  let prev = Patdiff_core.{ name = source_path; text = source_content } in
  let next = Patdiff_core.{ name = source_path; text = result } in
  Compare_core.diff_strings ~print_global_header:true configuration ~prev ~next
  |> function
  | `Different diff -> Some diff
  | `Same -> None
