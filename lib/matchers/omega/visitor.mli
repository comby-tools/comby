open Angstrom

open Types

class state : object
  (* val location *)
  (* val visited *)
end

class virtual syntax : object
  method virtual user_defined_delimiters : (string * string) list
  method virtual escapable_string_literals : Syntax.escapable_string_literals option
  method virtual raw_string_literals : (string * string) list
  method virtual comments : Syntax.comment_kind list
end

(** We can't quite have a pure visitor that accumulates over 'a because it's
    hard to extract out the fix and 'many' parser loop to propagate acc from
    previous states and hand it back to the user. However, we can let the user
    express a change (map) or accumulation over 'a list, which is how this
    visitor works. The 'enter' methods communicate that this is not a 'true'
    visit (there is no super#visit necessary or supported--none of the entered
    terms recursively visit other terms, that is all driven by generate_parser.
*)
class virtual ['a] visitor : object
  inherit state
  inherit syntax

  method enter_delimiter : string -> string -> 'a list -> 'a list
  method enter_spaces : string -> 'a list
  method enter_other : string -> 'a list
  method enter_hole : Hole.t -> 'a list
  method enter_toplevel : 'a list -> 'a list

  (** Unfortunately we have to expose Angstrom.t to make this accessible and
      reusable to sublcasses. If it weren't for this we wouldn't need to expose
      it. *)
  method comment_parser : string t

  method run : string -> 'a list
end

val visit : 'a #visitor -> string -> 'a list
