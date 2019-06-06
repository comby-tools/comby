open Core
open Patdiff_lib

open Configuration

(* This is the default patdiff configuration. *)
let default =
  sprintf
    {|;; -*- scheme -*-
;; patdiff Configuration file

(
 (context 16)

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

let terminal () =
  Config.t_of_sexp (Sexp.of_string default)
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
