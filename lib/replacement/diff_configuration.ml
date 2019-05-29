open Core
open Patdiff_lib

open Configuration


let terminal () =
  Config.t_of_sexp (Sexp.of_string default)
  |> parse

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

 (line_unified
  ((prefix ((text "!|") (style ())))))

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
