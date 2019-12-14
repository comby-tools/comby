open Angstrom
open Core_kernel

open Types

let debug = true

let source_ref : string ref = ref ""

let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let skip_unit p =
  p >>| ignore

let cons x xs = x :: xs

(* run p until t succeeds *)
let many_till_returning_till (p : 'a t) (t : 'b t) : 'b t =
  fix (fun m -> t <|> (p *> m))

(* FIXME use skip_while once everything works, we don't need the string *)
let spaces1 =
  satisfy is_whitespace >>= fun c ->
  take_while is_whitespace >>= fun s ->
  return (Format.sprintf "%c%s" c s)

let alphanum =
  satisfy (function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9' -> true
      | _ -> false)

module Make (S : Syntax.S) (Info : Info.S) = struct
  include Info
  open Types

  let syntax_record =
    Syntax.{ user_defined_delimiters = S.user_defined_delimiters
           ; escapable_string_literals = S.escapable_string_literals
           ; raw_string_literals = S.raw_string_literals
           ; comments = S.comments
           }

  class make_syntax
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

  module Printer = struct
    class printer = object(_)
      inherit make_syntax syntax_record
      inherit [string] Visitor.visitor

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
      Visitor.visit (new printer) match_template
  end

  type almost_omega_match_production =
    { offset : int
    ; identifier : string
    ; text : string
    }
  [@@deriving yojson]

  (* A simple production type that only saves matches *)
  type production =
    | Parsed
    | Hole_match of almost_omega_match_production
    | Match of Match.t
    | Deferred of hole

  module Matcher = struct

    let make_unit p =
      p >>= fun _ -> return Parsed

    let zero =
      fail ""

    let comment_parser comments =
      match comments with
      | [] -> zero
      | syntax ->
        List.map syntax ~f:(function
            | Types.Syntax.Multiline (left, right) ->
              let module M = Parsers.Comments.Omega.Multiline.Make(struct
                  let left = left
                  let right = right
                end)
              in
              M.comment
            | Until_newline start ->
              let module M = Parsers.Comments.Omega.Until_newline.Make(struct
                  let start = start
                end)
              in
              M.comment
            | Nested_multiline (_, _) -> zero) (* FIXME unimplemented nested multiline comments *)
        |> choice

    let escapable_string_literal_parser escapable_string_literals =
      (match escapable_string_literals with
       | None -> []
       | Some Types.Syntax.{ delimiters; escape_character } ->
         List.map delimiters ~f:(fun delimiter ->
             let module M =
               Parsers.String_literals.Omega.Escapable.Make(struct
                 let delimiter = delimiter
                 let escape = escape_character
               end)
             in
             M.base_string_literal >>= fun contents ->
             (* FIXME figure out if we need to do the same callback thing here to communicate
                forward that we entered a string *)
             return contents
           ))
      |> choice

    class generator = object(self)
      inherit make_syntax syntax_record
      inherit [production Angstrom.t] Visitor.visitor

      method !enter_other other =
        [make_unit @@ string other]

      (* Includes swallowing comments for now. See template_visitor. *)
      method !enter_spaces _ =
        [ make_unit @@ spaces1 ]

      (* Apply rules here *)
      method enter_hole_match (matched : almost_omega_match_production) : production =
        Hole_match matched

      (* Apply rules here *)
      method enter_match (matched: Match.t) : production =
        Match matched

      (* Wrap the hole so we can find it in enter_delimiter *)
      method !enter_hole ({ sort; identifier; _ } as hole) =
        match sort with
        | Everything -> [return (Deferred hole)]
        | Alphanum ->
          let result =
            pos >>= fun pos_before ->
            many1 ((alphanum <|> char '_') >>| String.of_char)
            >>= fun matched ->
            let text = String.concat matched in
            return (self#enter_hole_match ({ offset = pos_before; identifier; text }))
          in
          [result]

        | _ -> failwith "TODO"

      method !enter_delimiter left right body =
        (* call the hole converter here... *)
        [make_unit @@ string left] @ body @ [make_unit @@ string right]

      (* Once we're at the toplevel of the template, generate the source
         matcher by prefixing it with the 'skip' part. Make it record
         a match_context when satisfied *)
      method !enter_toplevel template_elements =
        Format.printf "Entered toplevel@.";
        let prefix = choice
            [ skip_unit (comment_parser self#comments)
            ; skip_unit (escapable_string_literal_parser self#escapable_string_literals)
            ; skip_unit any_char
            ] in
        let record_match () : production t =
          let open Match in
          pos >>= fun start_position ->
          List.fold template_elements
            ~init:(return (Match.Environment.create ()))
            ~f:(fun acc parser ->
                acc >>= fun acc ->
                parser >>= function
                | Hole_match { offset; identifier; text } ->
                  let before = Location.default in (* FIXME. after - len(text)? *)
                  let after = { Location.default with offset } in
                  let range = Range.{ match_start = before; match_end = after } in
                  let acc = Environment.add ~range acc identifier text in
                  return acc
                | _ -> return acc
              )
          >>= fun environment ->
          pos >>= fun end_position ->
          let match_start = Location.{ default with offset = start_position } in
          let match_end = Location.{ default with offset = end_position } in
          let range = Range.{ match_start; match_end } in
          let extract_matched_text
              source
              Location.{ offset = match_start; _ }
              Location.{ offset = match_end; _ } =
            String.slice source match_start match_end
          in
          let matched = extract_matched_text !source_ref match_start match_end in
          Format.printf "Matched: %s@." matched;
          return (self#enter_match { matched; environment; range })
        in
        let result : production t =
          (many_till_returning_till
             prefix
             (at_end_of_input >>= function
               | true ->
                 Format.printf "Done -> Exit, end of input reached!@.";
                 fail "Done -> Exit"
               | false ->
                 Format.printf "Not at the end. Attempting recording match@.";
                 record_match ()
             ))
          <|> (fail "Failed" >>= fun _ -> Format.printf "Failed"; fail "Failed")
        in
        (* We're only looking for one *)
        let hd_production =
          many result >>= function
          | [] ->
            Format.printf "nothing@.";
            fail "none"
          | hd::_ ->
            Format.printf "hd@.";
            return hd
        in
        [hd_production]
    end

    let run match_template =
      Visitor.visit (new generator) match_template
  end

  module Engine = struct
    let run source p =
      let state = Buffered.parse p in
      let state = Buffered.feed state (`String source) in
      let state = Buffered.feed state `Eof in
      match state with
      | Buffered.Done ({ len; off; _ }, result) ->
        if len <> 0 then
          (if debug then
             Format.eprintf "Input left over in parse where not expected: off(%d) len(%d)" off len;
           Or_error.error_string "Does not match template")
        else
          Ok result
      | _ -> Or_error.error_string "No matches"
  end

  let fold source p_list ~f ~init =
    let accumulator =
      List.fold p_list ~init:(return init) ~f:(fun acc parser ->
          acc >>= fun acc ->
          parser >>= fun parsed ->
          return (f acc parsed))
    in
    Engine.run source accumulator

  let all ?configuration:_ ~template ~source : Match.t list =
    source_ref := source;
    let production_parsers = Matcher.run template in
    fold source production_parsers ~init:[] ~f:(fun acc ->
        function
        | Match m ->
          Format.printf "Yessir@.";
          m::acc
        | _ -> acc)
    |> function
    | Ok matches ->
      Format.printf "Returning %d matches@." @@ List.length matches;
      matches
    | Error e ->
      Format.printf "Error: %s@." (Error.to_string_hum e);
      []

  let first ?configuration ?shift:_ template source : Match.t Or_error.t =
    match all ?configuration ~template ~source with
    | [] -> Or_error.error_string "nothing"
    | hd::_  -> Ok hd
end
