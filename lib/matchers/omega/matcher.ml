open Angstrom
open Core_kernel

open Types

let debug = false

let source_ref : string ref = ref ""

(* XXX can shortcircuit *)
(* what if you hit a reserved
   seqence "{" and then attempt
   ":[[" and then say "end of
   input" and then move ahead any_char. not good.
   going from longest to shortest works though *)
let any_char_except ~reserved =
  List.fold reserved
    ~init:(return `OK)
    ~f:(fun acc reserved_sequence ->
        option `End_of_input
          (peek_string (String.length reserved_sequence)
           >>= fun s ->
           if s = reserved_sequence then
             return `Reserved_sequence
           else
             acc))
  >>= function
  | `OK -> any_char
  | `End_of_input -> any_char
  | `Reserved_sequence -> fail "reserved sequence hit"

let between left right p =
  left *> p <* right

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
  type atom =
    | Parsed
    | Hole_match of almost_omega_match_production
    | Match of Match.t

  type 'a production =
    | Atom of 'a
    | List of 'a list

  type result =
    | Parser of atom production Angstrom.t
    | Placeholder of hole

  module Matcher = struct

    let make_unit p =
      p >>= fun _ -> return (Atom Parsed)

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
      inherit [result] Visitor.visitor

      method !enter_other other =
        [ Parser (make_unit @@ string other) ]

      (* Includes swallowing comments for now. See template_visitor. *)
      method !enter_spaces _ =
        [ Parser (make_unit @@ spaces1) ]

      (* Apply rules here *)
      method enter_hole_match (matched : almost_omega_match_production) : atom production =
        Atom (Hole_match matched)

      (* Apply rules here *)
      method enter_match (matched: Match.t) : atom production =
        if debug then Format.printf "Matched: %s@." matched.matched;
        Atom (Match matched)

      (* Wrap the hole so we can find it in enter_delimiter *)
      method !enter_hole ({ sort; identifier; _ } as hole) =
        match sort with
        | Everything ->
          if debug then Format.printf "Creating placeholder@.";
          [ Placeholder hole ] (* we have to process this relative to the nesting level *)
        | Alphanum ->
          let result =
            pos >>= fun pos_before ->
            many1 ((alphanum <|> char '_') >>| String.of_char)
            >>= fun matched ->
            let text = String.concat matched in
            return (self#enter_hole_match ({ offset = pos_before; identifier; text }))
          in
          [Parser result]

        | _ -> failwith "TODO"

      (** TODO: move this into visitor *)
      method escapable_string_literal_parser =
        (match self#escapable_string_literals with
         | None -> []
         | Some { delimiters; escape_character } ->
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

      method until_of_from from =
        self#user_defined_delimiters
        |> List.find_map ~f:(fun (from', until) -> if from = from' then Some until else None)
        |> function
        | Some until -> until
        | None -> assert false

      method generate_greedy_hole_parser
          ?priority_left_delimiter:left_delimiter
          ?priority_right_delimiter:right_delimiter
          () =
        let between_nested_delims p from =
          let until = self#until_of_from from in
          between (string from) (string until) p
          >>= fun result -> return (String.concat @@ [from] @ result @ [until])
        in
        let between_nested_delims p =
          (match left_delimiter, right_delimiter with
           | Some left_delimiter, Some right_delimiter -> [ (left_delimiter, right_delimiter) ]
           | _ -> self#user_defined_delimiters)
          |> List.map ~f:fst
          |> List.map ~f:(between_nested_delims p)
          |> choice
        in
        let reserved =
          (match left_delimiter, right_delimiter with
           | Some left_delimiter, Some right_delimiter -> [ (left_delimiter, right_delimiter) ]
           | _ -> self#user_defined_delimiters)
          |> List.concat_map ~f:(fun (from, until) -> [from; until])
        in
        fix (fun grammar ->
            let delimsx = between_nested_delims (many grammar) in
            let other = any_char_except ~reserved >>| String.of_char in
            (* FIXME holes does not handle space here, but does in alpha *)
            choice
              [ self#comment_parser
              ; self#escapable_string_literal_parser
              (*; spaces1*) (* shouldn't be needed, reserved is locally constrained*)
              ; delimsx
              ; other
              ])

      method sequence_chain (p_list : result list) : atom production Angstrom.t =
        List.fold p_list ~init:(return (Atom Parsed)) ~f:(fun acc -> function
            | Parser p -> acc *> p
            | _ -> acc)

      (** TODO: can this not return a result, but instead just the parser? *)
      method convert_holes ~is_toplevel (p_list : result list) : result list =
        if debug then Format.printf "Time to sequence chain@.";
        let i = ref 0 in
        let init : result list = [] in
        List.fold_right p_list ~init ~f:(fun p acc ->
            let result =
              match p with
              | Placeholder { sort; identifier; _ } ->
                if debug then Format.printf "A placeholder@.";
                begin
                  match sort with
                  | Everything ->
                    if debug then Format.printf "Everything do hole %s@." identifier;
                    let first_pos = Set_once.create () in
                    let want =
                      if !i = 0 then
                        begin
                          if is_toplevel then
                            (
                              if debug then Format.printf "No rest, toplevel end of input@.";
                              (* needed for toplevel, but not nested. *)
                              let until = end_of_input >>= fun _ -> return (Atom Parsed) in
                              (many_till
                                 (pos >>= fun pos -> Set_once.set_if_none first_pos [%here] pos;
                                  self#generate_greedy_hole_parser ())
                                 (pos >>= fun pos -> Set_once.set_if_none first_pos [%here] pos;
                                  until)
                              )
                            )
                          else
                            (
                              (* eat everything in this level, there is no until *)
                              if debug then Format.printf "No rest, nested@.";
                              (pos >>= fun pos ->
                               Set_once.set_if_none first_pos [%here] pos;
                               many (self#generate_greedy_hole_parser ()))
                            )
                        end
                      else
                        (
                          if debug then Format.printf "There is a rest, sequence acc@.";
                          let until = self#sequence_chain acc in
                          (many_till
                             (pos >>= fun pos -> Set_once.set_if_none first_pos [%here] pos;
                              self#generate_greedy_hole_parser ())
                             (pos >>= fun pos -> Set_once.set_if_none first_pos [%here] pos;
                              until)
                          )
                        )
                    in
                    let pparser =
                      want
                      >>| String.concat
                    in
                    let parser_to_match_result =
                      pparser >>= fun text ->
                      let offset =
                        match Set_once.get first_pos with
                        | Some offset -> offset
                        | _ -> failwith "Did not expect unset offset"
                      in
                      if debug then Format.printf "Matched %S...@." text;
                      return (self#enter_hole_match { offset; identifier; text })
                    in
                    (* merged with acc, which is consumed *)
                    [Parser parser_to_match_result]
                  | _ -> (* already handled *)
                    acc
                end
              | _ -> p::acc
            in
            i := !i + 1;
            result)


      method !enter_delimiter left right body =
        if debug then Format.printf "Entering delimiter@.";
        let parsers =
          [ Parser (make_unit @@ string left)]
          @ self#convert_holes ~is_toplevel:false body
          @ [ Parser (make_unit @@ string right) ]
        in
        parsers

      (* Once we're at the toplevel of the template, generate the source
         matcher by prefixing it with the 'skip' part. Make it record
         a match_context when satisfied *)
      method !enter_toplevel template_elements =
        if debug then Format.printf "Entered toplevel@.";
        let prefix = choice
            [ skip_unit (comment_parser self#comments)
            ; skip_unit (escapable_string_literal_parser self#escapable_string_literals)
            ; skip_unit any_char
            ] in
        let record_match () : atom production t =
          let open Match in
          pos >>= fun start_position ->
          (* toplevel hole conversion *)
          let template_elements = self#convert_holes ~is_toplevel:true template_elements in
          List.fold template_elements
            ~init:(return (Match.Environment.create ()))
            ~f:(fun acc result ->
                acc >>= fun acc ->
                let parser =
                  match result with
                  | Parser p -> p
                  | Placeholder _ ->
                    failwith "all place holders should be converted by now"
                in
                parser >>= function
                | Atom Hole_match { offset; identifier; text } ->
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
          return (self#enter_match { matched; environment; range })
        in
        let result : atom production t =
          (many_till_returning_till
             prefix
             (at_end_of_input >>= function
               | true ->
                 if debug then Format.printf "Done -> Exit, end of input reached!@.";
                 (* can't not fail here, or it never terminates *)
                 fail "Done -> Exit"
               | false ->
                 if debug then Format.printf "Not at the end. Attempting recording match@.";
                 record_match ()
             ))
          <|> (fail "Failed" >>= fun _ -> Format.printf "No match, did not hit end of input"; fail "Failed")
        in
        let production : atom production t =
          many result >>= function
          | [] ->
            if debug then Format.printf "no results seen at end of parse at toplevel@.";
            fail "none"
          | matches ->
            if debug then Format.printf "# matches: %d@." @@ List.length matches;
            let collect_matches =
              List.fold matches ~init:[] ~f:(fun acc m ->
                  match m with
                  | Atom ((Match _) as x) -> x::acc
                  | _ -> acc)
            in
            return (List collect_matches)
        in
        [Parser production]
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
           (* instead of reporting an error, just give the results as far as we got *)
           (*Or_error.error_string "Does not match template"*)
           Ok result)
        else
          Ok result
      | _ -> Or_error.error_string "No matches"
  end

  let fold source p_list ~f ~init =
    let accumulator =
      List.fold p_list ~init:(return init) ~f:(fun acc result ->
          acc >>= fun acc ->
          let parser =
            match result with
            | Parser p -> p
            | Placeholder _ ->
              failwith "all place holders should be converted by now"
          in
          parser >>= fun parsed ->
          return (f acc parsed))
    in
    Engine.run source accumulator

  let all ?configuration:_ ~template ~source : Match.t list =
    source_ref := source;
    let production_parsers = Matcher.run template in
    fold source production_parsers ~init:[] ~f:(fun acc ->
        function
        | List matches ->
          List.fold matches ~init:acc ~f:(fun acc ->
              function
              | Match m -> m::acc
              | _ -> acc)
        | _ -> acc)
    |> function
    | Ok matches ->
      if debug then Format.printf "Returning %d matches@." @@ List.length matches;
      matches
    | Error e ->
      if debug then Format.printf "Error: %s@." (Error.to_string_hum e);
      []

  let first ?configuration ?shift:_ template source : Match.t Or_error.t =
    match all ?configuration ~template ~source with
    | [] -> Or_error.error_string "nothing"
    | hd::_  -> Ok hd
end
