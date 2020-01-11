open Core

open Angstrom

open Parser
open Types
open Omega

let configuration_ref = ref (Configuration.create ())
let matches_ref : Match.t list ref = ref []
let source_ref : string ref = ref ""
let current_environment_ref : Match.Environment.t ref = ref (Match.Environment.create ())

let (|>>) p f =
  p >>= fun x -> return (f x)

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let rewrite =
  Sys.getenv "REWRITE"
  |> Option.is_some

let actual = Buffer.create 10

let rewrite_template = ref ""

let substitute template env =
  let substitution_formats =
    [ ":[ ", "]"
    ; ":[", ".]"
    ; ":[", "\\n]"
    ; ":[[", "]]"
    ; ":[", "]"
    (* optional syntax *)
    ; ":[? ", "]"
    ; ":[ ?", "]"
    ; ":[?", ".]"
    ; ":[?", "\\n]"
    ; ":[[?", "]]"
    ; ":[?", "]"
    ]
  in
  Match.Environment.vars env
  |> List.fold ~init:(template, []) ~f:(fun (acc, vars) variable ->
      match Match.Environment.lookup env variable with
      | Some value ->
        List.find_map substitution_formats ~f:(fun (left,right) ->
            let pattern = left^variable^right in
            if Option.is_some (String.substr_index template ~pattern) then
              Some (String.substr_replace_all acc ~pattern ~with_:value, variable::vars)
            else
              None)
        |> Option.value ~default:(acc,vars)
      | None -> acc, vars)

let record_match_context pos_before pos_after =
  let open Match.Location in
  if debug then Format.printf "match context start pos: %d@." pos_before;
  if debug then Format.printf "match context end pos %d@." pos_after;
  let extract_matched_text source { offset = match_start; _ } { offset = match_end; _ } =
    String.slice source match_start match_end
  in
  (* line/col values are placeholders and not accurate until processed in pipeline.ml *)
  let match_context =
    let match_start = { offset = pos_before; line = 1; column = pos_before + 1 } in
    let match_end = { offset = pos_after; line = 1; column = pos_after + 1 } in
    let text = extract_matched_text !source_ref match_start match_end in
    Match.
      { range = { match_start; match_end }
      ; environment = !current_environment_ref
      ; matched = text
      }
  in
  (* substitute now *)
  if debug then Format.printf "Curr env: %s@." @@ Match.Environment.to_string !current_environment_ref;
  let result, _ = substitute !rewrite_template !current_environment_ref in
  (* Don't just append, but replace the match context including constant
     strings. I.e., somewhere where we are appending the parth that matched, it
     shouldn't, and instead just ignore. *)
  if rewrite then Buffer.add_string actual result;
  matches_ref := match_context :: !matches_ref

module Make (Syntax : Syntax.S) (Info : Info.S) = struct
  include Info

  (* This is the init we will pass in with a functor later *)
  let acc = ""

  (* This is the function we will pass in with a functor later *)
  let f acc (production : production) =
    match production with
    | String s -> (* unmatched, append when we rewrite *)
      if rewrite then Buffer.add_string actual s;
      acc
    | Template_string _ -> acc (* matched. if a constant string in the template is matched, don't append it *)
    | Unit -> if debug then Format.printf "Unit@."; acc
    | Hole _ -> if debug then Format.printf "Hole@."; acc
    | Match _ ->
      if debug then Format.printf "Match@.";
      acc

  let r acc production : (production * 'a) t =
    let open Match in
    let open Location in
    let open Range in
    let acc = f acc production in
    match production with
    | String s ->
      if debug then Format.printf "Matched String: %S@." s;
      return (Unit, acc)
    | Match { offset = pos_begin; identifier; text = content } ->
      if debug then Format.printf "Match: %S @@ %d for %s@." content pos_begin identifier;
      (* line/col values are placeholders and not accurate until processed in pipeline.ml *)
      let before = { offset = pos_begin; line = 1; column = pos_begin + 1 } in
      let pos_after_offset = pos_begin + String.length content in
      let after = { offset = pos_after_offset; line = 1; column = pos_after_offset + 1 } in
      let range = { match_start = before; match_end = after } in
      let add identifier = Environment.add ~range !current_environment_ref identifier content in
      let environment =
        match Environment.exists !current_environment_ref identifier with
        | true ->
          (* FIXME: get rid of UUID *)
          let fresh_hole_id =
            Format.sprintf "%s_%s_equal" Uuid_unix.(Fn.compose Uuid.to_string create ()) identifier
          in
          add fresh_hole_id
        | false -> add identifier
      in
      current_environment_ref := environment;
      return (Unit, acc)
    | _ -> return (Unit, acc)


  let multiline left right =
    let open Parsers.Comments.Omega.Multiline in
    let module M = Make(struct let left = left let right = right end) in
    M.comment

  let until_newline start =
    let open Parsers.Comments.Omega.Until_newline in
    let module M = Make(struct let start = start end) in
    M.comment

  let comment_parser =
    match Syntax.comments with
    | [] -> zero
    | syntax ->
      let parsers =
        List.map syntax ~f:(function
            | Multiline (left, right) -> multiline left right
            | Until_newline start -> until_newline start
            | Nested_multiline (_, _) -> zero) (* FIXME: unimplemented nested multiline comments *)
      in
      choice parsers

  type 'a literal_parser_callback = contents:string -> left_delimiter:string -> right_delimiter:string -> 'a

  let escapable delimiter escape_character =
    let open Parsers.String_literals.Omega.Escapable in
    let module M = Make(struct let delimiter = delimiter let escape = escape_character end) in
    M.base_string_literal

  let raw left_delimiter right_delimiter =
    let open Parsers.String_literals.Omega.Raw in
    let module M = Make(struct let left_delimiter = left_delimiter let right_delimiter = right_delimiter end) in
    M.base_string_literal

  let escapable_string_literal_parser (f : 'a literal_parser_callback) =
    choice @@
    match Syntax.escapable_string_literals with
    | None -> []
    | Some { delimiters; escape_character } ->
      List.map delimiters ~f:(fun delimiter ->
          escapable delimiter escape_character >>= fun contents ->
          return (f ~contents ~left_delimiter:delimiter ~right_delimiter:delimiter))

  let raw_string_literal_parser (f : 'a literal_parser_callback) =
    choice @@
    List.map Syntax.raw_string_literals ~f:(fun (left_delimiter, right_delimiter) ->
        raw left_delimiter right_delimiter >>= fun contents ->
        return (f ~contents ~left_delimiter ~right_delimiter))

  let until_of_from from =
    Syntax.user_defined_delimiters
    |> List.find_map ~f:(fun (from', until) -> if from = from' then Some until else None)
    |> function
    | Some until -> until
    | None -> assert false

  module Deprecate = struct
    let reserved_delimiters =
      List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) -> [from; until])
      |> List.append [":["; "]"]
      |> List.append [":[["; "]]"]

    let reserved =
      reserved_delimiters @ [" "; "\n"; "\t"; "\r"]
      |> List.sort ~compare:(fun v2 v1 ->
          String.length v1 - String.length v2)
  end

  let reserved_parsers =
    let user_defined_delimiters = List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) -> [from; until]) in
    let user_defined_escapable_strings =
      match Syntax.escapable_string_literals with
      | Some { delimiters; _ } ->
        List.concat_map delimiters ~f:(fun delimiter -> [delimiter])
      | None -> []
    in
    let user_defined_raw_strings =
      List.concat_map Syntax.raw_string_literals ~f:(fun (from, until) -> [from; until])
    in
    let hole_syntax = [ ":["; "]"; ":[["; ":]]" ] in
    let spaces = [ " "; "\n"; "\t"; "\r" ] in
    let reserved =
      user_defined_delimiters
      @ user_defined_escapable_strings
      @ user_defined_raw_strings
      @ hole_syntax
      @ spaces
    in
    choice @@ List.map reserved ~f:string

  let generate_single_hole_parser () =
    (alphanum <|> char '_') |>> String.of_char

  let generate_everything_hole_parser
      ?priority_left_delimiter:left_delimiter
      ?priority_right_delimiter:right_delimiter
      () =
    let between_nested_delims p from =
      let until = until_of_from from in
      between (string from) (string until) p
      >>= fun result -> return (String.concat @@ [from] @ result @ [until])
    in
    let between_nested_delims p =
      let parsers =
        match left_delimiter, right_delimiter with
        | Some left_delimiter, Some right_delimiter -> [ (left_delimiter, right_delimiter) ]
        | _ -> Syntax.user_defined_delimiters
      in
      parsers
      |> List.map ~f:fst
      |> List.map ~f:(between_nested_delims p)
      |> choice
    in
    let reserved =
      let parsers =
        match left_delimiter, right_delimiter with
        | Some left_delimiter, Some right_delimiter -> [ (left_delimiter, right_delimiter) ]
        | _ -> Syntax.user_defined_delimiters
      in
      List.concat_map parsers ~f:(fun (from, until) -> [from; until])
    in
    fix (fun grammar ->
        let delimsx = between_nested_delims (many grammar) in
        let other = Parser.Deprecate.any_char_except ~reserved |>> String.of_char in
        choice
          [ comment_parser
          ; raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
          ; escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
          ; spaces1
          ; delimsx
          ; other
          ])

  (* this thing is wrapped by a many. also rename it to 'string hole match syntax per char' *)
  let escapable_literal_grammar ~right_delimiter =
    match Syntax.escapable_string_literals with
    | None -> zero
    | Some { escape_character; _ } ->
      choice
        [ (string (Format.sprintf "%c%s" escape_character right_delimiter))
        ; (string (Format.sprintf "%c%c" escape_character escape_character))
        ; (Parser.Deprecate.any_char_except ~reserved:[right_delimiter] |>> String.of_char)
        ]

  let raw_literal_grammar ~right_delimiter =
    (Parser.Deprecate.any_char_except ~reserved:[right_delimiter] |>> String.of_char)

  let sequence_chain ?left_delimiter:_ ?right_delimiter (p_list : (production * 'a) t list) =
    if debug then Format.printf "Sequence chain p_list size: %d@." @@ List.length p_list;
    let i = ref 0 in
    List.fold_right p_list ~init:(return (Unit, acc)) ~f:(fun p acc ->
        let result =
          if debug then Format.printf "iterate fold_right %d@." !i;
          match parse_string p "_signal_hole" with
          | Error _ ->
            if debug then Format.printf "Composing p with terminating parser@.";
            p *> acc
          | Ok (Hole { sort; identifier; dimension; _ }, user_state) ->
            begin
              match sort with
              | Alphanum ->
                pos >>= fun offset ->
                many1 (generate_single_hole_parser ())
                >>= fun value ->
                (* acc must come after in order to sat. try mimic alpha to better express this. *)
                acc >>= fun _ ->
                let m =
                  { offset
                  ; identifier
                  ; text = String.concat value
                  }
                in
                r user_state (Match m)
              | Non_space ->
                if debug then Format.printf "Doing non_space@.";
                let first_pos = ref (-1) in
                let set_pos v = first_pos := v in
                let get_pos () = !first_pos in
                let rest =
                  (* if this is the base case (the first time we go around the
                     loop backwards, when the first parser is a hole), then it
                     means there's a hole at the end without anything following
                     it in the template. So it should always match to
                     end_of_input, not empty string. If it matches to empty
                     string it chops up the matches so that f,o,o are three
                     matches of foo. *)
                  if !i = 0 then
                    (if debug then Format.printf "hole until: match to the end of this level@.";
                     end_of_input)
                  else
                    (if debug then Format.printf "hole until: append suffix@.";
                     skip_unit acc)
                in
                (
                  pos >>= fun pos ->
                  if get_pos () = (-1) then set_pos pos;
                  let stop_at = choice [ rest; skip_unit reserved_parsers ] in
                  many1_till_stop any_char stop_at (* Beware of this use. *)
                )
                >>= fun value ->
                acc >>= fun _ ->
                let offset =
                  match get_pos () with
                  | -1 -> failwith "Did not expect unset offset"
                  | offset ->
                    if debug then Format.printf "Offset: %d@." offset;
                    set_pos (-1);
                    offset
                in
                let m =
                  { offset
                  ; identifier
                  ; text = String.of_char_list value
                  }
                in
                r user_state (Match m)
              | Line ->
                pos >>= fun offset ->
                let allowed =
                  many (Parser.Deprecate.any_char_except ~reserved:["\n"])
                  |>> fun x -> [(String.of_char_list x)^"\n"]
                in
                allowed <* char '\n' >>= fun value ->
                acc >>= fun _ ->
                let m =
                  { offset
                  ; identifier
                  ; text = String.concat value
                  }
                in
                r user_state (Match m)
              | Blank ->
                pos >>= fun offset ->
                many1 blank >>= fun value ->
                acc >>= fun _ ->
                let m =
                  { offset
                  ; identifier
                  ; text = String.of_char_list value
                  }
                in
                r user_state (Match m)
              | Everything ->
                if debug then Format.printf "do hole %s@." identifier;
                let first_pos = ref (-1) in
                let set_pos v = first_pos := v in
                let get_pos () = !first_pos in
                (* change this so that rest is not consumed *)
                let rest =
                  (* if this is the base case (the first time we go around the
                     loop backwards, when the first parser is a hole), then it
                     means there's a hole at the end without anything following
                     it in the template. So it should always match to
                     end_of_input (not empty string) *)
                  if !i = 0 then
                    (if debug then Format.printf "hole until: match to the end of this level@.";
                     end_of_input)
                  else
                    (if debug then Format.printf "hole until: append suffix@.";
                     skip_unit acc)
                in
                let hole_matcher =
                  (many_till
                     (pos >>= fun pos ->
                      if debug then Format.printf "Pos is %d@." pos;
                      if get_pos () = (-1) then set_pos pos;
                      (match dimension with
                       | Code -> generate_everything_hole_parser ()
                       | Escapable_string_literal ->
                         let right_delimiter = Option.value_exn right_delimiter in
                         escapable_literal_grammar ~right_delimiter
                       | Raw_string_literal ->
                         let right_delimiter = Option.value_exn right_delimiter in
                         escapable_literal_grammar ~right_delimiter
                       | _ -> failwith "Unimplemented for comment"
                      )
                     )
                     (pos >>= fun pos ->
                      if get_pos () = (-1) then set_pos pos;
                      if debug then Format.printf "Pos is %d@." pos;
                      rest)
                     (* it may be that the many till for the first parser
                        succeeds on 'empty string', specifically in the :[1]:[2]
                        case for :[1]. We won't capture the pos of :[1] in the
                        first parser since it doesn't fire, so we have to
                        set the pos right before the until parser below, if that
                        happens. *)
                  ) >>| String.concat
                in
                hole_matcher >>= fun text ->
                let offset =
                  match get_pos () with
                  | -1 -> failwith "Did not expect unset offset"
                  | offset ->
                    if debug then Format.printf "Offset: %d@." offset;
                    set_pos (-1);
                    offset
                in
                let m =
                  { offset
                  ; identifier
                  ; text
                  }
                in
                r user_state (Match m)
            end
          | Ok (_, _user_state) -> failwith "unreachable: _signal_hole parsed but not handled by Hole variant"
        in
        i := !i + 1;
        result)

  (* XXX change ignore to unit once everything works.
     right now it's the string that was parsed by spaces1 *)
  let generate_spaces_parser _ignored =
    (* XXX still some parts ignored in the choice case in Alpha *)
    if debug then Format.printf "Template_spaces(%s)@." _ignored;
    spaces1 >>= fun s1 ->
    many comment_parser >>= fun result ->
    spaces >>= fun s2 ->
    r acc (Template_string (s1^String.concat result^s2))

  (** All code can have comments interpolated *)
  let generate_string_token_parser str =
    if debug then Format.printf "Template_string(%s)@." str;
    many comment_parser
    >>= fun s1 ->
    string str >>= fun result ->
    many comment_parser >>= fun s2 ->
    r acc (Template_string (String.concat s1 ^ result ^ String.concat s2))

  let single_hole_parser () =
    string ":[[" *> identifier_parser () <* string "]]"

  let everything_hole_parser () =
    string ":[" *> identifier_parser () <* string "]"

  let non_space_hole_parser () =
    string ":[" *> identifier_parser () <* string ".]"

  let line_hole_parser () =
    string ":[" *> identifier_parser () <* string "\\n]"

  let blank_hole_parser () =
    string ":["
    *> many1 blank
    *> identifier_parser ()
    <* string "]"

  let hole_parser sort dimension : (production * 'a) t t =
    let open Hole in
    let hole_parser =
      match sort with
      | Alphanum -> single_hole_parser ()
      | Everything -> everything_hole_parser ()
      | Blank -> blank_hole_parser ()
      | Line -> line_hole_parser ()
      | Non_space -> non_space_hole_parser ()
    in
    let skip_signal hole = skip_unit (string "_signal_hole") |>> fun () -> (Hole hole, acc) in
    hole_parser |>> fun identifier -> skip_signal { sort; identifier; dimension; optional = false }

  let reserved_holes () =
    [ single_hole_parser ()
    ; everything_hole_parser ()
    ; non_space_hole_parser ()
    ; line_hole_parser ()
    ; blank_hole_parser ()
    ]

  let generate_hole_for_literal sort ~contents ~left_delimiter ~right_delimiter () =
    let literal_holes =
      Hole.sorts ()
      |> List.map ~f:(fun kind -> hole_parser kind sort) (* Note: Uses attempt in alpha *)
      |> choice
    in
    let _reserved_holes =
      reserved_holes ()
      |> List.map ~f:skip_unit
      |> choice
    in
    let parser =
      many @@
      choice
        [ literal_holes
        ; ((many1 (Parser.Deprecate.any_char_except ~reserved:[":["]) |>> String.of_char_list)
           |>> generate_string_token_parser)
        ]
    in
    match parse_string parser contents with
    | Ok parsers -> sequence_chain ~left_delimiter ~right_delimiter parsers
    | Error _ ->
      failwith "If this failure happens it is a bug: Converting a \
                quoted string in the template to a parser list should \
                not fail here"

  let general_parser_generator : (production * 'a) t t =
    let spaces : (production * 'a) t t = spaces1 |>> generate_spaces_parser in
    let other =
      (many1 (Parser.Deprecate.any_char_except ~reserved:Deprecate.reserved) |>> String.of_char_list)
      |>> generate_string_token_parser
    in
    let code_holes =
      Hole.sorts ()
      |> List.map ~f:(fun kind -> hole_parser kind Code)
      |> choice
    in
    fix (fun (generator : (production * 'a) t list t) ->
        if debug then Format.printf "Descends@.";
        let nested =
          if debug then Format.printf "Nested@.";
          choice @@
          List.map Syntax.user_defined_delimiters ~f:(fun (left_delimiter, right_delimiter) ->
              (string left_delimiter *> generator <* string right_delimiter)
              >>= fun (g: (production * 'a) t list) ->
              if debug then Format.printf "G size: %d; delim %s@." (List.length g) left_delimiter;
              return @@
              sequence_chain @@
              [string left_delimiter >>= fun result -> r acc (Template_string result)]
              @ g
              @ [ string right_delimiter >>= fun result -> r acc (Template_string result)])
        in
        many @@ choice
          [ code_holes
          ; raw_string_literal_parser (generate_hole_for_literal Raw_string_literal ())
          ; escapable_string_literal_parser (generate_hole_for_literal Escapable_string_literal ())
          ; spaces
          ; nested
          ; other
          ]
        >>= fun x ->
        if debug then Format.printf "Produced %d parsers in main generator@." @@ List.length x;
        return x
      )
    |>> fun p_list ->
    match p_list with
    | [] ->
      (* The template is the empty string and source is nonempty. We need to
         detect it here or we will always match successfully on empty string and
         never advance input below. *)
      r acc Unit
    | p_list ->
      p_list
      |> sequence_chain
      |> fun matcher ->
      match !configuration_ref.match_kind with
      | Exact ->
        pos >>= fun start_pos ->
        if debug then Format.printf "Yes exact@.";
        matcher >>= fun _access_last_production_here ->
        pos >>= fun end_pos ->
        end_of_input >>= fun _ ->
        record_match_context start_pos end_pos;
        current_environment_ref := Match.Environment.create ();
        r acc Unit
      | Fuzzy ->
        let prefix =
          choice
            [ comment_parser
            ; (raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents))
            ; (escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents))
            ; any_char |>> Char.to_string
            ]
        in
        (* many1 may be appropriate *)
        let matches =
          many @@
          many_till (prefix >>= fun s -> r acc (String s))
            begin
              at_end_of_input >>= fun at_end ->
              if debug then Format.printf "We are at the end? %b.@." at_end;
              if at_end then fail "end"
              else
                (* We may have found a match *)
                pos >>= fun start_pos ->
                let matched =
                  matcher >>= fun production ->
                  if debug then Format.printf "Full match context result@.";
                  pos >>= fun end_pos ->
                  record_match_context start_pos end_pos;
                  current_environment_ref := Match.Environment.create ();
                  return production
                in
                let no_match =
                  (* Reset any partial binds of holes in environment. *)
                  if debug then Format.printf "Failed to match and not at end.@.";
                  current_environment_ref := Match.Environment.create ();
                  (* cannot return: we must try some other parser or else we'll
                     infini loop! We can't advance because we haven't
                     successfuly parsed the character at the current position.
                     So: fail and try another parser in the choice. *)
                  fail "no match, try something else"
                in
                choice [ matched; no_match ]
            end
        in
        matches >>= fun _result ->
        r acc Unit

  let to_template template =
    let state = Buffered.parse general_parser_generator in
    let state = Buffered.feed state (`String template) in
    Buffered.feed state `Eof
    |> function
    | Buffered.Done ({ len; buf; _ }, p) ->
      let c = Bigarray.Array1.unsafe_get buf (len - 1) in
      if len <> 0 then failwith @@ Format.sprintf "Input left over in template where not expected: %d: %c" len c;
      Ok p
    | _ -> Or_error.error_string "Template could not be parsed."

  let run_the_parser_for_first p source : Match.t Or_error.t =
    source_ref := source;
    let state = Buffered.parse p in
    let state = Buffered.feed state (`String source) in
    let state = Buffered.feed state `Eof in
    match state with
    | Buffered.Done ({ len; off; _ }, (_, _result_string)) ->
      if rewrite then Format.eprintf "Result string:@.---@.%s---@." @@ Buffer.contents actual;
      if len <> 0 then
        (if debug then Format.eprintf "Input left over in parse where not expected: off(%d) len(%d)" off len;
         Or_error.error_string "Does not match template")
      else
        Ok (Match.create ()) (* Fake for now *)
    | _ -> Or_error.error_string "No matches"

  let first_is_broken ?configuration:_ ?shift:_ template source : Match.t Or_error.t =
    match to_template template with
    | Error e -> Error e
    | Ok p ->
      begin match run_the_parser_for_first p source with
        | Ok _ -> (* May have matches, ok to access *)
          begin
            match !matches_ref with
            | [] -> Or_error.error_string "Empty matches"
            | hd::_ -> Ok hd
          end
        | Error e -> (* Matching failed *)
          Error e
      end

  let set_rewrite_template rewrite_template' =
    rewrite_template := rewrite_template'

  (** Hardcoded case when template and source are empty string. The parser logic
      is too tricky for this right now. *)
  let trivial =
    let open Match in
    let open Location in
    let open Range in
    let location =
      { offset = 0
      ; line = 1
      ; column = 1
      }
    in
    let range =
      { match_start = location
      ; match_end = location
      }
    in
    Match.create ~range ()

  let all ?configuration ~template ~source : Match.t list =
    configuration_ref := Option.value configuration ~default:!configuration_ref;
    matches_ref := [];
    if template = "" && source = "" then [trivial]
    else match first_is_broken template source with
      | Ok _
      | Error _ -> List.rev !matches_ref

  let first ?configuration ?shift:_ template source : Match.t Or_error.t =
    configuration_ref := Option.value configuration ~default:!configuration_ref;
    matches_ref := [];
    match all ?configuration ~template ~source with
    | [] -> Or_error.error_string "No result"
    | (hd::_) -> Ok hd (* FIXME be efficient *)
end
