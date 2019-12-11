open Angstrom
open Core_kernel

open Types

class syntax
    Syntax.{ user_defined_delimiters
           ; escapable_string_literals
           ; raw_string_literals
           ; comments
           } = object(_)
  method user_defined_delimiters = user_defined_delimiters
  method escapable_string_literals = escapable_string_literals
  method raw_string_literals = raw_string_literals
  method comments = comments
end

let default_syntax =
  Syntax.{ user_defined_delimiters = [ "(", ")" ]
         ; escapable_string_literals = None
         ; raw_string_literals = []
         ; comments = []
         }


module Printer = struct
  class printer = object(_)
    inherit syntax default_syntax
    inherit [string] Template_visitor.visitor

    method !enter_other other =
      [Format.sprintf "Other: %s@." other]

    method !enter_spaces spaces =
      [Format.sprintf "Spaces: %s@." spaces]

    method !enter_hole { identifier; _ } =
      [Format.sprintf "Hole: :[%s]@." identifier]

    method !enter_delimiter left right body =
      [Format.sprintf "Delim_open: %s@.\t%sDelim_close: %s@." left (String.concat ~sep:"\t" body) right]

    method !enter_toplevel parsed =
      [Format.sprintf "Toplevel: %s@." (String.concat ~sep:"," parsed)]
  end

  let run match_template =
    Template_visitor.fold (new printer) match_template
end

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let spaces1 =
  satisfy is_whitespace >>= fun c ->
  (* XXX use skip_while once everything works.
     we don't need the string *)
  take_while is_whitespace >>= fun s ->
  return (Format.sprintf "%c%s" c s)


module Matcher = struct

  type omega_match_production =
    { offset : int
    ; identifier : string
    ; text : string
    }

  (* A simple production type that only saves matches *)
  type production =
    | Unit
    | Match of omega_match_production
    | Intermediate_hole of hole

  let make_unit p =
    p >>= fun _ -> return Unit

  class generator = object(_)
    inherit syntax default_syntax
    inherit [production Angstrom.t] Template_visitor.visitor

    method !enter_other other =
      [make_unit @@ string other]

    method !enter_spaces _ =
      (* Add comments, see matcher.ml *)
      [make_unit @@ spaces1]

    (* Wrap the hole so we can find it in enter_delimiter *)
    method !enter_hole hole =
      [return (Intermediate_hole hole)]

    method !enter_delimiter left right body =
      [make_unit @@ string left] @ body @ [make_unit @@ string right]
      (* here we know that holes could be in the level, and where sequence_chain would go. we can merge into a single parser if desired. But don't merge it! What we want is to parse parse parse each little part and call a callback like 'parsed_string' ...*)

  end

  let run match_template =
    Template_visitor.fold (new generator) match_template
end



(* Proof of concept *)
module Dumb_generator = struct
  open Angstrom

  class generator = object(_)
    inherit syntax default_syntax
    inherit ['a Angstrom.t] Template_visitor.visitor

    method !enter_other other =
      [string other]

    (* too strong! *)
    method !enter_spaces spaces =
      [string spaces]

    (* hard *)
    method !enter_hole _ = []


    method !enter_delimiter left right body =
      [string left] @ body @ [string right]

  end

  let run match_template =
    Template_visitor.fold (new generator) match_template
end
