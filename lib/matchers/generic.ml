(** Dyck with common string literals. *)
module Syntax = struct
  include Dyck.Syntax

  let escapable_string_literals =
    [ {|"|}
    ; {|'|}
    ]

  let escape_char =
    '\\'
end

include Matcher.Make(Syntax)
