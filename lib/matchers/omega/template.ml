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
      let msg = Format.sprintf "Other: %s@." other in
      [msg]

    method !enter_spaces spaces =
      let msg = Format.sprintf "Spaces: %s@." spaces in
      [msg]

    method !enter_hole { identifier; _ } =
      let msg = Format.sprintf "Hole: :[%s]@." identifier in
      [msg]

    method !enter_delimiter left right body =
      let msg = Format.sprintf "Delim_open: %s@.\t%sDelim_close: %s@." left (String.concat ~sep:"\t" body) right in
      [msg]
  end

  let run match_template =
    Template_visitor.fold (new printer) match_template
end

module Parser_generator = struct
  open Angstrom

  class generator = object(_)
    inherit syntax default_syntax
    inherit ['a Angstrom.t] Template_visitor.visitor

    method !enter_other other =
      [return (string other)]

    (* too strong! *)
    method !enter_spaces spaces =
      [return (string spaces)]

    (* hard *)
    method !enter_hole _hole =
      []

    method !enter_delimiter left right body =
      [return (string left)] @ body @ [return (string right)]

  end

  let run match_template =
    Template_visitor.fold (new generator) match_template
end
