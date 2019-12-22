open Core
open MParser

open Configuration
open Match
open Range
open Location
open Types

let configuration_ref = ref (Configuration.create ())
let weaken_delimiter_hole_matching = false

let debug =
  Sys.getenv "DEBUG_COMBY"
  |> Option.is_some

let debug_hole =
  Sys.getenv "DEBUG_COMBY_HOLE"
  |> Option.is_some

let debug_position =
  Sys.getenv "DEBUG_COMBY_POS"
  |> Option.is_some

let f _ = return Unit

let extract_matched_text source { offset = match_start; _ } { offset = match_end; _ } =
  String.slice source match_start match_end

let is_not p s =
  if is_ok (p s) then
    Empty_failed (unknown_error s)
  else
    match read_char s with
    | Some c -> Consumed_ok (c, advance_state s 1, No_error)
    | None -> Empty_failed (unknown_error s)

type 'a literal_parser_callback = contents:string -> left_delimiter:string -> right_delimiter:string -> 'a
type 'a nested_delimiter_callback = left_delimiter:string -> right_delimiter:string -> 'a

module Make (Syntax : Syntax.S) (Info : Info.S) = struct
  include Info

  let escapable_string_literal_parser (f : 'a literal_parser_callback) =
    (match Syntax.escapable_string_literals with
     | None -> []
     | Some { delimiters; escape_character } ->
       List.map delimiters ~f:(fun delimiter ->
           let module M =
             Parsers.String_literals.Alpha.Escapable.Make(struct
               let delimiter = delimiter
               let escape = escape_character
             end)
           in
           M.base_string_literal >>= fun contents ->
           return (f ~contents ~left_delimiter:delimiter ~right_delimiter:delimiter)))
    |> choice

  let raw_string_literal_parser (f : 'a literal_parser_callback) =
    List.map Syntax.raw_string_literals ~f:(fun (left_delimiter, right_delimiter) ->
        let module M =
          Parsers.String_literals.Alpha.Raw.Make(struct
            let left_delimiter = left_delimiter
            let right_delimiter = right_delimiter
          end)
        in
        M.base_string_literal >>= fun contents ->
        return (f ~contents ~left_delimiter ~right_delimiter))
    |> choice

  let comment_parser =
    match Syntax.comments with
    | [] -> MParser.zero
    | syntax ->
      List.map syntax ~f:(function
          | Multiline (left, right) ->
            let module M =
              Parsers.Comments.Alpha.Multiline.Make(struct
                let left = left
                let right = right
              end)
            in
            M.comment
          | Nested_multiline (left, right)  ->
            let module M =
              Parsers.Comments.Alpha.Nested_multiline.Make(struct
                let left = left
                let right = right
              end)
            in
            M.comment
          | Until_newline start ->
            let module M =
              Parsers.Comments.Alpha.Until_newline.Make(struct
                let start = start
              end)
            in
            M.comment)
      |> choice

  let escapable_literal_grammar ~right_delimiter =
    match Syntax.escapable_string_literals with
    | None -> zero
    | Some { escape_character; _ } ->
      (string (Format.sprintf "%c%s" escape_character right_delimiter))
      <|>
      (string (Format.sprintf "%c%c" escape_character escape_character))
      <|> (is_not (string right_delimiter) |>> String.of_char)

  let raw_literal_grammar ~right_delimiter =
    is_not (string right_delimiter) |>> String.of_char

  let generate_spaces_parser () =
    (* At least a space followed by comments and spaces. *)
    (spaces1
     >> many comment_parser << spaces
     >>= fun result -> f result)
    <|>
    (* This case not covered by tests, may not be needed. *)
    (many1 comment_parser << spaces >>= fun result -> f result)

  let sequence_chain (plist : ('c, Match.t) parser sexp_list) : ('c, Match.t) parser =
    List.fold plist ~init:(return Unit) ~f:(>>)

  let with_debug_matcher s tag =
    if debug then
      match tag with
      | `Position tag ->
        if debug_position then
          let prev = prev_char s in
          let curr = read_char s in
          let next = next_char s in
          let print_if = function
            | Some s -> s
            | None -> '?'
          in
          Format.printf "Position Tag: %s@." tag;
          Format.printf "H_prev: %c H_curr: %c H_next: %c@."
            (print_if prev)
            (print_if curr)
            (print_if next)
      | `Delimited delimited ->
        Format.printf "<d>%s</d>%!" delimited
      | `Delimited_suffix suffix ->
        Format.printf "<d_s>%s</d_s>%!" suffix
      | `Checkpoint (label, s) ->
        Format.printf "Point(%s):<d>%s</d>" label s
      | _ -> assert false

  let is_alphanum delim = Pcre.(pmatch ~rex:(regexp "^[[:alnum:]]+$") delim)
  let whitespace : (id, Match.t) parser = many1 space |>> String.of_char_list
  let not_alphanum = many1 (is_not alphanum) |>> String.of_char_list
  let reserved_alphanum_delimiter_must_satisfy =
    Syntax.user_defined_delimiters
    |> List.filter_map ~f:(fun (from, until) ->
        if not (is_alphanum from) then
          Some [from; until]
        else
          None)
    |> List.concat
    |> List.map ~f:string
    |> List.map ~f:attempt

  let nested_delimiters_parser (f : 'a nested_delimiter_callback) =
    (* All alphanum delimiter fixups happen in the generated parser, not here. *)
    let between p from until s =
      (string from >>= fun from ->
       if debug then with_debug_matcher s (`Delimited from);
       p >>= fun p_result ->
       string until >>= fun until ->
       if debug then with_debug_matcher s (`Delimited until);
       return p_result)
        s
    in
    Syntax.user_defined_delimiters
    |> List.map ~f:(fun (left_delimiter, right_delimiter) ->
        between
          (f ~left_delimiter ~right_delimiter)
          left_delimiter
          right_delimiter
      )
    |> choice
    (* Backtrack on failure, specifically for alphanum. *)
    |> attempt

  (** Generates a word that is NOT OK with having alphanum chars before or after
      it. This disables substring parsing. *)
  let generate_word chars : ('c, _) parser =
    (fun s ->
       let prev = prev_char s in
       (match prev with
        | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
        | _ ->
          string (String.of_char_list chars)
          >>= fun result ->
          (* there has to be at least one "not alphanum" after this parser for it to succeed, or eof. *)
          look_ahead (skip (many1 (is_not alphanum)) <|> eof) >>= fun _ ->
          f result) s)

  let generate_string_token_parser str : ('c, _) parser =
    many comment_parser
    >> string str
    >> many comment_parser
    >>= fun result -> f result

  let is_optional () =
    opt false (char '?' |>> fun _ -> true)

  let identifier () =
    (many (alphanum <|> char '_') |>> String.of_char_list)

  let hole_body () =
    is_optional () >>= fun optional ->
    identifier () >>= fun identifier ->
    return (optional, identifier)

  let everything_hole_parser () =
    string ":[" >> hole_body () << string "]"

  let non_space_hole_parser () =
    string ":[" >> hole_body () << string ".]"

  let line_hole_parser () =
    string ":[" >> hole_body () << string "\\n]"

  let blank_hole_parser () =
    string ":[" >>
    is_optional () >>= fun optional ->
    (many1 blank)
    >> identifier () >>= fun identifier ->
    string "]" >>
    return (optional, identifier)

  let alphanum_hole_parser () =
    string ":[[" >> hole_body () << string "]]"

  let reserved_holes () =
    let alphanum = alphanum_hole_parser () |>> snd in
    let everything = everything_hole_parser () |>> snd in
    let non_space = non_space_hole_parser () |>> snd in
    let blank = blank_hole_parser () |>> snd in
    let line = line_hole_parser () |>> snd in
    [ non_space
    ; line
    ; blank
    ; alphanum
    ; everything
    ]

  let reserved_delimiters () =
    let required_from_suffix = not_alphanum in
    let required_until_suffix = not_alphanum in
    let handle_alphanum_delimiters_reserved_trigger from until =
      let from_parser =
        fun s ->
          (match prev_char s with
           | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
           | _ ->
             string from >>= fun from ->
             look_ahead required_from_suffix >>= fun _ ->
             return from) s
      in
      let until_parser s =
        (string until >>= fun until ->
         eof <|> look_ahead (skip required_until_suffix) >>= fun _ ->
         (* if current char/next_char is alphanum, make unsat. *)
         let prev = prev_char s in
         if debug then with_debug_matcher s (`Position "reserved_delimiter_until");
         match prev with
         | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
         | _ -> return until
        )
          s
      in
      [from_parser; until_parser]
    in
    let reserved_delimiters =
      List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) ->
          if is_alphanum from && is_alphanum until then
            handle_alphanum_delimiters_reserved_trigger from until
          else
            [ string from; string until])
    in
    let reserved_escapable_strings =
      match Syntax.escapable_string_literals with
      | Some { delimiters; _ } ->
        List.concat_map delimiters ~f:(fun delimiter -> [delimiter])
        |> List.map ~f:string
      | None -> []
    in
    let reserved_raw_strings =
      List.concat_map Syntax.raw_string_literals ~f:(fun (from, until) -> [from; until])
      |> List.map ~f:string
    in
    reserved_holes ()
    @ reserved_delimiters
    @ reserved_escapable_strings
    @ reserved_raw_strings
    |> List.map ~f:skip
    (* Attempt the reserved: otherwise, if a parser partially succeeds,
       it won't detect that single or greedy is reserved. *)
    |> List.map ~f:attempt
    |> choice

  let reserved _s =
    reserved_delimiters ()
    <|> skip (space |>> Char.to_string)

  let until_of_from from =
    Syntax.user_defined_delimiters
    |> List.find_map ~f:(fun (from', until) -> if from = from' then Some until else None)
    |> function
    | Some until -> until
    | None -> assert false

  let record_matches identifier p : ('c, Match.t) parser =
    get_pos >>= fun (pre_index, pre_line, pre_column) ->
    p >>= fun matched ->
    get_pos >>= fun (post_index, post_line, post_column) ->
    update_user_state
      (fun ({ Match.environment; _ } as result) ->
         if debug then begin
           Format.printf "Updating user state:@.";
           Format.printf "%s |-> %s@." identifier (String.concat matched);
           Format.printf "ID %s: %d:%d:%d - %d:%d:%d@."
             identifier
             pre_index pre_line pre_column
             post_index post_line post_column;
         end;
         let pre_location : Location.t =
           Location.
             { offset = pre_index
             ; line = pre_line
             ; column = pre_column
             }
         in
         let post_location : Location.t =
           Location.
             { offset = post_index
             ; line = post_line
             ; column = post_column
             }
         in
         let range = { match_start = pre_location; match_end = post_location } in
         let environment =
           if Environment.exists environment identifier then
             let fresh_hole_id =
               Format.sprintf "%s_%s_equal" Uuid_unix.(Fn.compose Uuid.to_string create ()) identifier
             in
             Environment.add ~range environment fresh_hole_id (String.concat matched)
           else
             Environment.add ~range environment identifier (String.concat matched)
         in
         { result with environment })
    >>= fun () -> f matched

  let alphanum_delimiter_must_satisfy =
    many1 (is_not (skip (choice reserved_alphanum_delimiter_must_satisfy) <|> skip alphanum))
    |>> String.of_char_list

  let generate_everything_hole_parser
      ?priority_left_delimiter:left_delimiter
      ?priority_right_delimiter:right_delimiter =
    let with_debug_hole tag =
      match tag with
      | `Spaces chars ->
        if debug_hole then Format.printf "<H_sp>";
        return (String.of_char_list chars)
      | `Delimited delimited ->
        if debug_hole then Format.printf "<H_d>%s</H_d>" delimited;
        return delimited
      | `Character c ->
        if debug_hole then Format.printf "<H_c>%c</H_c>" c;
        return (String.of_char c)
      | `Body body ->
        let body = String.concat body in
        if debug_hole then Format.printf "<H_body>%s</H_body>" body;
        return body
      | `Checkpoint (label, s) ->
        if debug_hole then Format.printf "Point(%s):<d>%s</d>" label s;
        return s
      | _ -> assert false
    in
    let delimiters =
      if weaken_delimiter_hole_matching then
        match left_delimiter, right_delimiter with
        | Some left_delimiter, Some right_delimiter ->
          [ (left_delimiter, right_delimiter) ]
        | _ -> Syntax.user_defined_delimiters
      else
        Syntax.user_defined_delimiters
    in
    let handle_alphanum_delimiters from until p =
      (* mandatory_prefix: needs to be not alphanum AND not non-alphanum delim.
         If it is a paren, we need to fail and get out of this alphanum block
         parser (how did we end up in here? because we said that we'd accept
         anything as prefix to 'def', including '(', and so '(' is not handled
         as a delim.) *)
      let mandatory_prefix = alphanum_delimiter_must_satisfy in
      (* mandatory_suffix: be more strict with suffix of opening delimiter: don't
         use 'any non-alphanum', but instead use whitespace. This since 'def;'
         is undesirable, and 'def.foo' may be intentional. But 'end.' or 'end;'
         probably still refer to a closing delim. *)
      let mandatory_suffix = choice reserved_alphanum_delimiter_must_satisfy <|> whitespace in
      let satisfy_opening_delimiter prev =
        (match prev with
         | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
         (* Try parse whitespace, and we want to capture its length, in case
            this is a space between, like 'def def end end'. But in the case
            where there's no space, it means we have just entered the beginning
            of the hole which may start with the 'd' of 'def', but since we
            already know that the previous char is not alphanum in this branch
            (so it is a delimiter or whitespace) it is OK to continue: in this
            case, return "". *)
         | _ -> mandatory_prefix <|> return "")
        >>= fun prefix ->
        string from >>= fun open_delimiter ->
        with_debug_hole (`Checkpoint ("open_delimiter_<pre>"^prefix^"</pre>_sat_for", open_delimiter)) >>= fun _ ->
        (* Use look_ahead to ensure that there is, e.g., whitespace after this
           possible delimiter, but without consuming input. Whitespace needs to
           not be consumed so that we can detect subsequent delimiters. *)
        look_ahead mandatory_suffix >>= fun suffix ->
        with_debug_hole (`Checkpoint ("open_delimiter_<suf>"^suffix^"</suf>_sat_for", open_delimiter)) >>= fun _ ->
        return (prefix, open_delimiter)
      in
      let satisfy_closing_delimiter =
        string until >>= fun close_delimiter ->
        look_ahead @@ mandatory_suffix >>= fun suffix ->
        with_debug_hole (`Checkpoint ("close_delimiter_<suf>"^suffix^"</suf>_sat_for", close_delimiter)) >>= fun _ ->
        return close_delimiter
      in
      (fun s ->
         let prev = prev_char s in
         (satisfy_opening_delimiter prev >>= fun (prefix, open_delimiter) ->
          p >>= fun in_between ->
          with_debug_hole (`Body in_between) >>= fun in_between ->
          satisfy_closing_delimiter >>= fun close_delimiter ->
          return
            ((prefix^open_delimiter)
             ^in_between
             ^close_delimiter)) s)
      (* Use attempt so that, e.g., 'struct' is tried after 'begin' delimiters under choice. *)
      |> attempt
    in
    let handle_alphanum_delimiters_reserved_trigger from until =
      (* If it's alphanum, only consider it reserved if there is, say, whitespace after and so
         handle alternatively. Otherwise, return empty to indicate 'this sequence of characters
         is not reserved'. *)
      let reserved =
        Syntax.user_defined_delimiters
        |> List.filter_map ~f:(fun (from, _) ->
            if not (is_alphanum from) then
              Some from
            else
              None)
        |> List.map ~f:string
        |> List.map ~f:attempt
      in
      let required_delimiter_terminal =
        many1 (is_not (skip (choice reserved) <|> skip alphanum)) |>> String.of_char_list
      in
      List.map [from; until] ~f:(fun delimiter ->
          (fun s ->
             let prev = prev_char s in
             (match prev with
              | Some prev when is_alphanum (Char.to_string prev) ->
                (* If prev is alphanum, this can't possibly be a reserved delimiter. Just continue. *)
                fail "unsat"
              | _ -> string delimiter >>= fun _ ->
                look_ahead required_delimiter_terminal) s))
    in
    (* The cases for which we need to stop parsing just characters
       and consider delimiters. *)
    let reserved =
      List.concat_map delimiters ~f:(fun (from, until) ->
          if is_alphanum from then
            handle_alphanum_delimiters_reserved_trigger from until
          else
            [string from; string until]
        )
      |> List.map ~f:attempt
      |> choice
    in
    (* A parser that understands the hole matching cut off points happen at
       delimiters. *)
    let rec nested_grammar s =
      (comment_parser
       <|> raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
       <|> escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
       <|> (many1 space >>= fun r -> with_debug_hole (`Spaces r))
       <|> (attempt @@ delims_over_holes >>= fun r -> with_debug_hole (`Delimited r))
       (* Only consume if not reserved. If it is reserved, we want to trigger the 'many'
          in (many nested_grammar) to continue. *)
       <|> (is_not (reserved <|> (space |>> Char.to_string)) >>= fun r -> with_debug_hole (`Character r)))
        s
    and delims_over_holes s =
      let between_nested_delims p =
        let capture_delimiter_result p ~from =
          let until = until_of_from from in
          if is_alphanum from then
            handle_alphanum_delimiters from until p
          else
            between (string from) (string until) p
            >>= fun result -> return (String.concat @@ [from] @ result @ [until])
        in
        delimiters
        |> List.map ~f:(fun pair -> capture_delimiter_result p ~from:(fst pair))
        |> choice
      in
      (between_nested_delims (many nested_grammar)) s
    in
    nested_grammar

  let coalesce_whitespace prefix_parser suffix_parser =
    let is_whitespace p =
      match
        parse_string p " " (Match.create ()),
        (* suffix parser could be a hole. It needs to fail on
           parsing something like "X" to be a whitespace parser *)
        parse_string p "X" (Match.create ())
      with
      | Success _, Failed _ -> true
      | _ -> false
    in
    let pre = is_whitespace prefix_parser in
    if debug then Format.printf "Pre: %b@." pre;
    let suf = is_whitespace suffix_parser in
    if debug then Format.printf "Suf: %b@." suf;
    pre && suf

  let prefix_parser p_list i =
    match List.nth (List.rev p_list) (i+1) with
    | Some p -> p
    | None ->
      if debug then Format.printf "Prefix parser unsat@.";
      fail "unsat"

  let turn_holes_into_matchers_for_this_level ?left_delimiter ?right_delimiter p_list =
    List.foldi (List.rev p_list) ~init:[] ~f:(fun i acc p ->
        match parse_string p "_signal_hole" (Match.create ()) with
        | Failed _ -> p::acc
        | Success Hole { sort; identifier; optional; dimension } ->
          begin
            match sort with
            | Alphanum ->
              let allowed =  choice [alphanum; char '_'] |>> String.of_char in
              let hole_semantics = many1 allowed in
              begin match optional with
                | false -> (record_matches identifier hole_semantics)::acc
                | true ->
                  if debug then Format.printf "Optional active@.";
                  match acc with
                  | [] ->
                    let hole_semantics = opt [] (attempt hole_semantics) in
                    (record_matches identifier hole_semantics)::acc
                  | (suffix::rest) as acc ->
                    let optional_succeeds_parser =
                      record_matches identifier hole_semantics
                      >> sequence_chain acc
                    in
                    let optional_fails_parser =
                      record_matches identifier (return []) >>= fun _ ->
                      if coalesce_whitespace (prefix_parser p_list i) suffix then
                        sequence_chain rest
                      else
                        sequence_chain acc
                    in
                    [(attempt optional_succeeds_parser)
                     <|> optional_fails_parser]
              end

            | Non_space ->
              let allowed =
                [skip space; reserved_delimiters ()]
                |> choice
                |> is_not
                |>> Char.to_string
              in
              let rest =
                match acc with
                | [] -> eof >>= fun () -> f [""]
                | _ -> sequence_chain acc
              in
              let hole_semantics = many1 (not_followed_by rest "" >> allowed) in
              let hole_semantics =
                if not optional then
                  hole_semantics
                else
                  opt [] (attempt hole_semantics)
              in
              (record_matches identifier hole_semantics)::acc

            | Line ->
              let allowed =
                many (is_not (char '\n'))
                |>> fun x -> [(String.of_char_list x)^"\n"]
              in
              let hole_semantics = allowed << char '\n' in
              let hole_semantics =
                if not optional then
                  hole_semantics
                else
                  opt [] (attempt hole_semantics)
              in
              (record_matches identifier hole_semantics)::acc

            | Blank ->
              let allowed = blank |>> String.of_char in
              let hole_semantics = many1 allowed in
              let hole_semantics =
                if not optional then
                  hole_semantics
                else
                  opt [] (attempt hole_semantics)
              in
              (record_matches identifier hole_semantics)::acc

            | Everything ->
              let matcher =
                match dimension with
                | Code ->
                  generate_everything_hole_parser
                    ?priority_left_delimiter:left_delimiter
                    ?priority_right_delimiter:right_delimiter
                | Escapable_string_literal ->
                  let right_delimiter = Option.value_exn right_delimiter in
                  escapable_literal_grammar ~right_delimiter
                | Raw_string_literal ->
                  let right_delimiter = Option.value_exn right_delimiter in
                  raw_literal_grammar ~right_delimiter
                | Comment -> failwith "Unimplemented"
              in
              match optional with
              | false ->
                let rest =
                  match acc with
                  | [] -> eof >>= fun () -> f [""]
                  | _ -> sequence_chain acc
                in
                (* Continue until rest, but don't consume rest. acc will
                   propagate the rest that needs to be consumed. *)
                let hole_semantics = many (not_followed_by rest "" >> matcher) in
                (record_matches identifier hole_semantics)::acc
              | true ->
                if debug then Format.printf "Optional active@.";
                match acc with
                | [] ->
                  let rest = eof >>= fun () -> f [""] in
                  let hole_semantics = many (not_followed_by rest "" >> matcher) in
                  (* Try match ordinary hole semantics, but if the parser
                     fails, just let it pass which leads to assigning "" to
                     identifier *)
                  let hole_semantics = opt [] (attempt hole_semantics) in
                  (record_matches identifier hole_semantics)::acc
                | (suffix::rest) as acc ->
                  let after = sequence_chain acc in
                  let hole_semantics = many (not_followed_by after "" >> matcher) in
                  (* The logic goes: Try to match ordinary hole semantics, and
                     propagate acc if it succeeds. If ordinary semantics fail,
                     let it pass and coalesce whitespace with prefix/suffix if
                     needed and only propagate 'rest', since we remove the
                     suffix by coalescing. *)
                  let optional_succeeds_parser =
                    (* This parser can succeed but does not consume after.
                       After must still be consumed (acc must be propagated).
                    *)
                    record_matches identifier hole_semantics
                    >>= fun _ ->
                    if debug then Format.printf "Optional record succeeds.@.";
                    sequence_chain acc >>= fun r ->
                    if debug then Format.printf "Rest succeeds.@.";
                    return r
                  in
                  let optional_fails_parser =
                    if debug then Format.printf "Optional fail case@.";
                    (* The optional parser that kicks in if
                       optional_succeeds_parser fails. It does not consume
                       anything. *)
                    (record_matches identifier (return [])) >>= fun _ ->
                    (* Record matches succeeded for optional hole, empty
                       match. No going back now. Consume suffix if prefix and
                       suffix are whitespace and propagate rest. Otherwise,
                       propagate acc. *)
                    if coalesce_whitespace (prefix_parser p_list i) suffix then
                      sequence_chain rest
                    else
                      sequence_chain acc
                  in
                  [(attempt optional_succeeds_parser)
                   <|> optional_fails_parser]
          end
        | Success _ -> failwith "Hole expected")

  let hole_parser sort dimension =
    let open Hole in
    let hole_parser =
      match sort with
      | Everything -> everything_hole_parser ()
      | Non_space -> non_space_hole_parser ()
      | Line -> line_hole_parser ()
      | Blank -> blank_hole_parser ()
      | Alphanum -> alphanum_hole_parser ()
    in
    let skip_signal hole = skip (string "_signal_hole") |>> fun () -> Hole hole in
    hole_parser |>> fun (optional, identifier) -> skip_signal { sort; identifier; dimension; optional }

  let generate_hole_for_literal sort ~contents ~left_delimiter ~right_delimiter s =
    let holes =
      Hole.sorts ()
      |> List.map ~f:(fun kind -> attempt (hole_parser kind sort))
    in
    let reserved_holes =
      reserved_holes ()
      |> List.map ~f:skip
      |> List.map ~f:attempt
      |> choice
    in

    let p =
      many
        (choice holes
         <|> ((many1 (is_not (reserved_holes))
               |>> String.of_char_list) |>> generate_string_token_parser))
    in
    match parse_string p contents "" with
    | Success p ->
      begin
        match sort with
        | Escapable_string_literal
        | Raw_string_literal ->
          (turn_holes_into_matchers_for_this_level ~left_delimiter ~right_delimiter p
           |> sequence_chain) s
        | _ -> assert false
      end
    | Failed (_msg, _) ->
      failwith "literal parser did not succeed"

  let rec generate_parsers s =
    many (common s)

  and common _s =
    let holes =
      Hole.sorts ()
      |> List.map ~f:(fun kind -> attempt (hole_parser kind Code))
    in
    choice
      [ choice holes
      (* String literals are handled specially because match semantics change inside string delimiters. *)
      ; raw_string_literal_parser (generate_hole_for_literal Raw_string_literal)
      ; escapable_string_literal_parser (generate_hole_for_literal Escapable_string_literal)
      (* Nested delimiters are handled specially for nestedness. *)
      ; nested_delimiters_parser generate_outer_delimiter_parsers
      (* Whitespace is handled specially because we may change whether they are significant for matching. *)
      ; spaces1 |>> generate_spaces_parser
      (* Optional: parse identifiers and disallow substring matching *)
      ; if !configuration_ref.disable_substring_matching then many1 (alphanum <|> char '_') |>> generate_word else zero
      (* Everything else. *)
      ; (many1 (is_not (reserved _s)) >>= fun cl ->
         if debug then Format.printf "<cl>%s</cl>" @@ String.of_char_list cl;
         return @@ String.of_char_list cl)
        |>> generate_string_token_parser
      ]

  and generate_outer_delimiter_parsers ~left_delimiter ~right_delimiter s =
    let before s =
      begin
        if debug_hole then with_debug_matcher s (`Position "generate_outer_delimiter");
        let p =
          if is_alphanum left_delimiter then
            (* This logic is needed for cases where we say 'def :[1] end' in the template,
               and don't match partially on, say, 'adef body endq' in the underlying generated
               parser. *)
            let prev = prev_char s in
            match prev with
            | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
            | _ -> string left_delimiter
          else
            string left_delimiter
        in
        p >>= fun _ -> f [left_delimiter]
      end s
    in
    let after =
      let p =
        if is_alphanum right_delimiter then
          (* This handles the case for something like 'def body endly'. *)
          string right_delimiter >>= fun delim ->
          look_ahead @@ (eof <|> skip not_alphanum) >>= fun _ ->
          return delim
        else
          string right_delimiter
      in
      p >>= fun _ -> f [right_delimiter]
    in
    (generate_parsers s >>= fun p_list ->
     (turn_holes_into_matchers_for_this_level
        ~left_delimiter
        ~right_delimiter
        ([before] @ p_list @ [after])
      |> sequence_chain)
     |> return
    ) s

  let general_parser_generator s =
    let outer_p =
      generate_parsers s >>= fun p_list ->
      (* EOF of template is here. *)
      eof >> (* Result is unit so ignore. *)
      (* Customize the inner parser. *)
      let inner_p =
        let matcher : ('a, Match.t) parser =
          turn_holes_into_matchers_for_this_level p_list
          |> sequence_chain
        in
        let matcher : ('a, Match.t) parser =
          let with_positions (matcher : ('a, Match.t) parser) : ('a, Match.t) parser =
            get_pos >>= fun (pre_offset, pre_line, pre_column) ->
            matcher >>= fun _last_production ->
            get_pos >>= fun (post_offset, post_line, post_column) ->
            let match_start =
              { offset = pre_offset
              ; line = pre_line
              ; column = pre_column
              } in
            let match_end =
              { offset = post_offset
              ; line = post_line
              ; column = post_column
              }
            in
            let range = { match_start; match_end } in
            update_user_state (fun result -> { result with range })
            >> return Unit
          in
          with_positions matcher
        in
        match !configuration_ref.match_kind with
        | Exact -> matcher << eof
        | Fuzzy ->
          many
            (not_followed_by matcher "" >>
             (
               (* Respect grammar but ignore contents up to a match. *)
               skip comment_parser
               <|> skip (raw_string_literal_parser (fun ~contents:_ ~left_delimiter:_ ~right_delimiter:_ -> ()))
               <|> skip (escapable_string_literal_parser (fun ~contents:_ ~left_delimiter:_ ~right_delimiter:_ -> ()))
               <|> skip any_char)
            )
          >> matcher
      in
      return inner_p
    in
    outer_p s

  let to_template template : ('a, Match.t) MParser.t Or_error.t =
    (* Use a match type for state so we can reuse parsers for inner and outer. *)
    match parse_string general_parser_generator template (Match.create ()) with
    | Success p -> Ok p
    | Failed (msg, _) -> Or_error.error_string msg

  (** shift: start the scan in the source at an offset *)
  let first' shift p source : Match.t Or_error.t =
    let set_start_pos p = fun s -> p (advance_state s shift) in
    let p = set_start_pos p in
    match parse_string' p source (Match.create ()) with
    | Success (_, result) ->
      if source = "" then
        (* If source is empty and p succeeds, it's the trivial case. We set
           the result manually. *)
        Ok {
          result with
          range =
            { match_start = { offset = 0; line = 1; column = 1 }
            ; match_end = { offset = 0; line = 1; column = 1 }
            }
        }
      else
        Ok result
    | Failed (msg, _) -> Or_error.error_string msg

  let first ?configuration ?shift template source =
    let open Or_error in
    configuration_ref := Option.value configuration ~default:!configuration_ref;
    to_template template >>= fun p ->
    let shift =
      match shift with
      | Some s -> s
      | None -> 0
    in
    first' shift p source

  let all ?configuration ~template ~source:original_source : Match.t list =
    let open Or_error in
    configuration_ref := Option.value configuration ~default:!configuration_ref;
    let make_result = function
      | Ok ok -> ok
      | Error _ -> []
    in
    make_result @@ begin
      to_template template >>= fun p ->
      let p =
        if template = "" then
          MParser.(eof >> return Unit)
        else
          p
      in
      let rec aux acc shift =
        match first' shift p original_source with
        | Ok ({range = { match_start; match_end; _ }; _} as result) ->
          let shift = match_end.offset in
          let matched = extract_matched_text original_source match_start match_end in
          let result = { result with matched } in
          if shift >= String.length original_source then
            result :: acc
          else
            aux (result :: acc) shift
        | Error _ -> acc
      in
      let matches = aux [] 0 |> List.rev in
      (* TODO(RVT): reintroduce nested matches *)
      let compute_nested_matches matches = matches in
      let matches = compute_nested_matches matches in
      return matches
    end
end
