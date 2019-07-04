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
  match Sys.getenv "DEBUG" with
  | Some _ -> true
  | None -> false

let debug_hole =
  match Sys.getenv "DEBUG" with
  | Some _ -> true
  | None -> false

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

module Make (Syntax : Syntax.S) = struct
  let escapable_string_literal_parser (f : 'a literal_parser_callback) =
    List.map Syntax.escapable_string_literals ~f:(fun delimiter ->
        let module M =
          Parsers.String_literals.Escapable.Make(struct
            let delimiter = delimiter
            let escape = Syntax.escape_char
          end)
        in
        M.base_string_literal >>= fun contents ->
        return (f ~contents ~left_delimiter:delimiter ~right_delimiter:delimiter))
    |> choice

  let raw_string_literal_parser (f : 'a literal_parser_callback) =
    List.map Syntax.raw_string_literals ~f:(fun (left_delimiter, right_delimiter) ->
        let module M =
          Parsers.String_literals.Raw.Make(struct
            let left_delimiter = left_delimiter
            let right_delimiter = right_delimiter
          end)
        in
        M.base_string_literal >>= fun contents ->
        return (f ~contents ~left_delimiter ~right_delimiter))
    |> choice

  let comment_parser =
    match Syntax.comment_parser with
    | [] -> MParser.zero
    | syntax ->
      List.map syntax ~f:(function
          | Multiline (left, right) ->
            let module M =
              Parsers.Comments.Multiline.Make(struct
                let left = left
                let right = right
              end)
            in
            M.comment
          | Nested_multiline (left, right)  ->
            let module M =
              Parsers.Comments.Nested_multiline.Make(struct
                let left = left
                let right = right
              end)
            in
            M.comment
          | Until_newline start ->
            let module M =
              Parsers.Comments.Until_newline.Make(struct
                let start = start
              end)
            in
            M.comment)
      |> choice

  let escapable_literal_grammar ~right_delimiter =
    (attempt
       (char Syntax.escape_char
        >> string right_delimiter
        >>= fun s -> return (Format.sprintf "%c%s" Syntax.escape_char s))
    )
    <|>
    (attempt
       (char Syntax.escape_char
        >> char Syntax.escape_char
        >> return (Format.sprintf "%c%c" Syntax.escape_char Syntax.escape_char))
    )
    <|> (is_not (string right_delimiter) |>> String.of_char)

  let raw_literal_grammar ~right_delimiter =
    is_not (string right_delimiter) |>> String.of_char

  let generate_spaces_parser () =
    (* at least a space followed by comments and spaces *)
    (spaces1
     >> many comment_parser << spaces
     >>= fun result -> f result)
    <|>
    (* This case not covered by tests, may not be needed *)
    (many1 comment_parser << spaces >>= fun result -> f result)

  let sequence_chain (plist : ('c, Match.t) parser sexp_list) : ('c, Match.t) parser =
    List.fold plist ~init:(return Unit) ~f:(>>)

  let with_debug s tag =
    if debug_hole then
      match tag with
      | `Position tag ->
        let prev = prev_char s in
        let curr = read_char s in
        let next = next_char s in
        let print_if = function
          | Some s -> s
          | None -> '?'
        in
        Format.printf "Tag: %s@." tag;
        Format.printf "H_prev: %c H_curr: %c H_next: %c@."
          (print_if prev)
          (print_if curr)
          (print_if next)
      | `Delimited delimited ->
        Format.printf "<d>%s</d>%!" delimited
      | `Delimited_suffix suffix ->
        Format.printf "<d_s>%s</d_s>%!" suffix
      | _ -> assert false

  let is_alphanum delim = Pcre.(pmatch ~rex:(regexp "^[[:alnum:]]+$") delim)
  let whitespace : (id, Match.t) parser = many1 space |>> String.of_char_list

  let nested_delimiters_parser (f : 'a nested_delimiter_callback) =
    let _required_delimiter_terminal =
      many1 (is_not alphanum) >>= fun x -> return @@ String.of_char_list x
    in
    let between p from until =
      (fun s ->
         begin
           if debug then with_debug s (`Position "between_start");
           string from >>= fun from ->
           if debug then with_debug s (`Delimited from);
           (* lookahead should be anything except alphanum (that includes holes). *)
           (if is_alphanum from then look_ahead _required_delimiter_terminal else return "")
           >>= fun suffix -> if debug then with_debug s (`Delimited suffix);
           p >>= fun p_result ->
           (* can't parse whitespace because p above already would. 'look_behind' needed? *)
           (*(if is_alphanum until then (skip whitespace) else return ()) >>= fun _ ->*)
           (* do not consider until valid unless current char/token is like
              whitespace or non-alphanum delim or hole *)
           string until >>= fun until ->
           if debug then with_debug s (`Delimited until);
           (if is_alphanum until then eof <|> look_ahead (skip whitespace) (* or hole? *) else return ())
           >>= fun _suffix -> if debug then with_debug s (`Delimited "fixme suffix");
           return p_result
         end
           s)
    in
    Syntax.user_defined_delimiters
    |> List.map ~f:(fun (left_delimiter, right_delimiter) ->
        between
          (f ~left_delimiter ~right_delimiter)
          left_delimiter
          right_delimiter
      )
    |> choice
    (* backtrack on failure, specifically for alphanum *)
    |> attempt

  (** All code can have comments interpolated *)
  let generate_string_token_parser str : ('c, _) parser =
    many comment_parser
    >> string str
    >> many comment_parser
    >>= fun result -> f result

  let everything_hole_parser () =
    string ":[" >> (many (alphanum <|> char '_') |>> String.of_char_list) << string "]"

  let non_space_hole_parser () =
    string ":[" >> (many (alphanum <|> char '_') |>> String.of_char_list) << string ".]"

  let line_hole_parser () =
    string ":[" >> (many (alphanum <|> char '_') |>> String.of_char_list) << string "\\n]"

  let blank_hole_parser () =
    string ":[" >> (many1 blank) >> (many (alphanum <|> char '_') |>> String.of_char_list) << string "]"

  let alphanum_hole_parser () =
    string ":[[" >> (many (alphanum <|> char '_') |>> String.of_char_list) << string "]]"
    >>= fun id ->
    return id

  let reserved_holes () =
    let alphanum = alphanum_hole_parser () in
    let everything = everything_hole_parser () in
    let non_space = non_space_hole_parser () in
    let blank = blank_hole_parser () in
    let line = line_hole_parser () in
    [ non_space
    ; line
    ; blank
    ; alphanum
    ; everything
    ]

  let reserved_delimiters _s =
    let _required_delimiter_terminal =
      many1 (is_not alphanum) >>= fun x -> return @@ String.of_char_list x
    in
    let reserved_delimiters =
      List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) ->
          if is_alphanum from && is_alphanum until then
            [ (fun s ->
                  (_required_delimiter_terminal >>= fun _ ->
                   (* alphanum start must be prefixed non-alphanum delim, or nothing *)
                   string from >>= fun from ->
                   (* alphanum start must be followed by space or a non-alphanum delim *)
                   look_ahead _required_delimiter_terminal >>= fun _ ->
                   (*Format.printf "PASSED: %s@." from;*)
                   return from) s)
            ;
                (*if is_alphanum @@ Char.to_string @@ Option.value_exn x then
                  fail ""
                  else*)
                (fun s ->
                   let _prev = prev_char s in
                   let _curr = read_char s in
                   let _next = next_char s in
                   (string until >>= fun until ->
                    eof <|> look_ahead (skip _required_delimiter_terminal) >>= fun _ ->
                    (* if current char /next_char is alphanum, make unsat. *)
                    (*Format.printf "PASSED: %s@." until;
                      if debug then Format.printf "prev: %c curr: %c next: %c@."
                        (Option.value_exn _prev)
                        (Option.value_exn _curr)
                        (Option.value_exn _next);*)
                    if Option.is_some _prev && is_alphanum (Char.to_string (Option.value_exn _prev)) then
                      fail "no"
                    else
                      return until) s)
            ]
          else
            [ string from
            ; string until]
        )
    in
    let reserved_escapable_strings =
      List.concat_map Syntax.escapable_string_literals ~f:(fun x -> [x])
      |> List.map ~f:string
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
    (* attempt the reserved: otherwise, if something passes partially,
       it won't detect that single or greedy is reserved *)
    |> List.map ~f:attempt
    |> choice

  let reserved s =
    reserved_delimiters s
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
         let environment = Environment.add ~range environment identifier (String.concat matched) in
         { result with environment })
    >>= fun () -> f matched

  let generate_everything_hole_parser
      ?priority_left_delimiter:left_delimiter
      ?priority_right_delimiter:right_delimiter =
    let between_nested_delims p =
      let capture_delimiter_result p ~from =
        let until = until_of_from from in
        let p =
          if is_alphanum from then
            (* make this not so fucked *)
            let mandatory_prefix =
              let reserved =
                Syntax.user_defined_delimiters
                |> List.filter_map ~f:(fun (from, until) ->
                    if not (is_alphanum from) then
                      Some [from; until]
                    else
                      None)
                |> List.concat
                |> List.map ~f:string
                (* not tested, this doesn't need to be attempt for (), etc. but if that ever changed... *)
                |> List.map ~f:attempt
              in
              (* needs to be not alphanum AND not non-alphanum delim. if it is
                 a paren, we need to fail and get out of this alphanum block
                 parser (but why did we end up in here? because we said that
                 we'd accept anything as prefix to 'def', including '(', and
                 so '(' is not handled as a delim. )*)
              many1 (is_not (skip (choice reserved) <|> skip alphanum)) >>= fun x ->
              return @@ String.of_char_list x
            in
            let mandatory_suffix =
              let reserved =
                Syntax.user_defined_delimiters
                |> List.filter_map ~f:(fun (from, until) ->
                    if not (is_alphanum from) then
                      Some [from; until]
                    else
                      None)
                |> List.concat
                |> List.map ~f:string
                |> List.map ~f:attempt
              in
              (* be more strict with suffix of pening delimiter: don't use
                 'any non-alphanum', but instead use whitespace. because 'def;' is garbage,
                 and 'def.foo' may be intentional. but end. or end; probably is a closing delim *)
              (choice reserved
               <|> whitespace) >>= fun x ->
              return x
            in
            (* Use attempt so that, e.g., 'struct' is tried after 'begin' delimiters under choice. *)
            attempt @@
            (fun s ->
               (
                 let _prev = prev_char s in
                 let _curr = read_char s in
                 let _next = next_char s in
                 (*Format.printf "PASSED: %s@." until;
                   if debug_hole then Format.printf "H_prev: %c H_curr: %c H_next: %c@."
                     (Option.value_exn _prev)
                     (Option.value_exn _curr)
                     (Option.value_exn _next);
                 *)
                 (
                   if Option.is_some _prev && is_alphanum (Char.to_string (Option.value_exn _prev)) then
                     (* if prev char is alphanum, this can't possibly be a delim *)
                     fail "no"
                   else
                     (* try parse white space, and we want to cpature its
                        length, in case this is a space between, like 'def def
                        end end'. But in the case where there's no space, it
                        means we have just entered the beginning of the hole
                        which may start with the 'd' of 'def', but since we
                        already know that the previous char is not alphanum in this branch (so
                        it is a delimiter or whitespace) it is OK to
                        continue: in this case, return "" *)
                     mandatory_prefix
                     <|> return ""
                 ) >>= fun prefix_opening ->
                 if prefix_opening <> "" then
                   if debug_hole then Format.printf "Nabbed <cl>%s</cl>" prefix_opening;
                 (*if debug_hole then Format.printf "Hole: Past required delim terminal <whitespace>. trying: %s@." from;*)
                 string from >>= fun open_delimiter ->
                 if debug_hole then Format.printf "Hole: Past string<3: <d>%s</d>@." open_delimiter;
                 (* Use look_ahead to ensure that there is, e.g., whitespace after this
                    possible delimiter, but without consuming input. Whitespace needs to
                    not be consumed so that we can detect subsequent delimiters. *)
                 look_ahead @@ mandatory_suffix >>= fun _unconsumed_opening_suffix ->
                 (*if debug_hole then Format.printf "Hole: Past required delimiter terminal: <%s>@." _unconsumed_opening_suffix;*)
                 p >>= fun in_between ->
                 (* think whitespace needs to be tracked here, but p will swallow it, so can't :(. look behind? *)
                 if debug_hole then Format.printf "<body>%s</body>@." @@ String.concat in_between;
                 string until >>= fun close_delimiter ->
                 (* look_ahead untested *)
                 look_ahead @@ mandatory_suffix >>= fun _unconsumed_closing_suffix ->
                 if debug_hole then Format.printf "Past ending<d>%s</d>" close_delimiter;
                 return
                   ((prefix_opening^open_delimiter)
                    ^(String.concat in_between)
                    ^close_delimiter)) s)
          else
            between (string from) (string until) p
            >>= fun result -> return (String.concat @@ [from] @ result @ [until])
        in
        p
      in
      let if_weaken =
        if weaken_delimiter_hole_matching then
          match left_delimiter, right_delimiter with
          | Some left_delimiter, Some right_delimiter ->
            [ (left_delimiter, right_delimiter) ]
          | _ -> Syntax.user_defined_delimiters
        else
          Syntax.user_defined_delimiters
      in
      if_weaken
      |> List.map ~f:(fun pair -> capture_delimiter_result p ~from:(fst pair))
      |> choice
    in
    (* the cases for which we need to stop parsing just characters
       and consider delimiters *)
    let reserved =
      let weaken =
        if weaken_delimiter_hole_matching then
          match left_delimiter, right_delimiter with
          | Some left_delimiter, Some right_delimiter ->
            [ (left_delimiter, right_delimiter) ]
          | _ -> Syntax.user_defined_delimiters
        else
          Syntax.user_defined_delimiters
      in
      weaken
      |> List.concat_map ~f:(fun (from, until) ->
          if is_alphanum from then
            (* if it's alphanum, only consider it reserved if there is, say, whitespace after and so
               handle alternatively. otherwise, return empty to indicate 'this sequence of characters
               is not reserved' *)
            [(fun s ->
                (let _prev = prev_char s in
                 let _curr = read_char s in
                 let _next = next_char s in
                 (*Format.printf "Considering reserved at...@.";
                   if debug_hole then Format.printf "H_prev: %c H_curr: %c H_next: %c@."
                     (Option.value_exn _prev)
                     (Option.value_exn _curr)
                     (Option.value_exn _next);
                 *)
                 (* if _prev is alphanum, this can't possibly be a reserved delimiter. just continue *)
                 if Option.is_some _prev && is_alphanum (Char.to_string (Option.value_exn _prev)) then
                   (* if prev char is alphanum, this can't possibly be a delim *)
                   fail "no"
                 else
                   (* under other conditions : option1 : it is not alphanum, so
                      it was a ( or whitespace): this is (almost) sat, so just
                      return if so. it may *not* be sat if it is *not* followed
                      by the expected 'whitespace', or rather, non-alphanum AND
                      non alphanum delimiter. FIXME use skip. *)
                   let required_delimiter_terminal =
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
                     (* needs to be not alphanum AND not non-alphanum delim. if it is
                        a paren, we need to fail and get out of this alphanum block
                        parser (but why did we end up in here? because we said that
                        we'd accept anything as prefix to 'def', including '(', and
                        so '(' is not handled as a delim. )*)
                     many1 (is_not (skip (choice reserved) <|> skip alphanum)) >>= fun x ->
                     return @@ String.of_char_list x in
                   (* the following is hacky; shouldn't it include whitespace? think more carefully about it. *)
                   string from >>= fun _ -> look_ahead required_delimiter_terminal) s)
            ; (fun s -> (
                   let _prev = prev_char s in
                   if Option.is_some _prev && is_alphanum (Char.to_string (Option.value_exn _prev)) then
                     (* if prev char is alphanum, this can't possibly be a delim *)
                     fail "no"
                   else
                     (* similar case to above. FIXME use skip *)
                     let required_delimiter_terminal =
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
                       (* as above. get rid of copypasta *)
                       many1 (is_not (skip (choice reserved) <|> skip alphanum)) >>= fun x ->
                       return @@ String.of_char_list x in
                     string until >>= fun _ -> look_ahead (required_delimiter_terminal)
                 ) s)
            ]
          else
            [string from; string until]
        )
      (* untested, but likely should be attempt *)
      |> List.map ~f:attempt
      |> choice
    in
    (* a parser that understands the hole matching cut off points happen at
       delimiters *)
    let with_debug tag =
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
      | _ -> assert false
    in
    let rec nested_grammar s =
      (comment_parser
       <|> raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
       <|> escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
       <|> (many1 space >>= fun r -> with_debug (`Spaces r))
       <|> (attempt @@ delims_over_holes >>= fun r -> with_debug (`Delimited r))
       (* only consume if not reserved. because if it is reserved, we want to trigger the 'many'
          to continue below, in (many nested_grammar) *)
       <|> (is_not (reserved <|> (space |>> Char.to_string)) >>= fun c -> with_debug (`Character c)))
        s
    and delims_over_holes s =
      (between_nested_delims (many nested_grammar)) s
    in
    nested_grammar

  let turn_holes_into_matchers_for_this_level ?left_delimiter ?right_delimiter p_list =
    List.fold_right p_list ~init:[] ~f:(fun p acc ->
        let process_hole =
          match parse_string p "_signal_hole" (Match.create ()) with
          | Failed _ -> p
          | Success result ->
            let rest =
              match acc with
              | [] -> eof >>= fun () -> f [""]
              | _ -> sequence_chain acc
            in
            match result with
            | Hole Alphanum (identifier, _) ->
              let allowed =  choice [alphanum; char '_'] |>> String.of_char in
              (* if we collapse the not_followed_by part, we will disallow substring matching. *)
              let hole_semantics = many1 (not_followed_by rest "" >> allowed) in
              record_matches identifier hole_semantics

            | Hole Non_space (identifier, _dimension) ->
              let allowed = non_space |>> String.of_char in
              let hole_semantics = many1 (not_followed_by rest "" >> allowed) in
              record_matches identifier hole_semantics

            | Hole Line (identifier, _dimension) ->
              let allowed =
                let until_char = '\n' in
                let allowed = any_char |>> String.of_char in
                let allowed = (not_followed_by (char until_char) "" >> allowed) in
                allowed
              in
              let hole_semantics = many1 (not_followed_by rest "" >> allowed) in
              record_matches identifier hole_semantics

            | Hole Blank (identifier, _dimension) ->
              let allowed = blank |>> String.of_char in
              let hole_semantics = many1 allowed in
              record_matches identifier hole_semantics

            | Hole Everything (identifier, dimension) ->
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
              (* continue until rest, but don't consume rest. *)
              let hole_semantics = many (not_followed_by rest "" >> matcher) in
              record_matches identifier hole_semantics

            | _ -> failwith "Hole expected"
        in
        process_hole::acc)

  let hole_parser sort dimension =
    let skip_signal result = skip (string "_signal_hole") |>> fun () -> result in
    match sort with
    | `Everything ->
      everything_hole_parser () |>> fun id ->
      skip_signal (Hole (Everything (id, dimension)))
    | `Non_space ->
      non_space_hole_parser () |>> fun id ->
      skip_signal (Hole (Non_space (id, dimension)))
    | `Line ->
      line_hole_parser () |>> fun id ->
      skip_signal (Hole (Line (id, dimension)))
    | `Blank ->
      blank_hole_parser () |>> fun id ->
      skip_signal (Hole (Blank (id, dimension)))
    | `Alphanum ->
      alphanum_hole_parser () |>> fun id ->
      skip_signal (Hole (Alphanum (id, dimension)))

  let generate_hole_for_literal sort ~contents ~left_delimiter ~right_delimiter s =
    let holes =
      [`Everything; `Non_space; `Alphanum; `Line; `Blank]
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
      [`Everything; `Non_space; `Alphanum; `Line; `Blank]
      |> List.map ~f:(fun kind -> attempt (hole_parser kind Code))
    in
    choice holes
    (* string literals are handled specially because match semantics change inside string delimiters *)
    <|> (raw_string_literal_parser (generate_hole_for_literal Raw_string_literal))
    <|> (escapable_string_literal_parser (generate_hole_for_literal Escapable_string_literal))
    (* nested delimiters are handled specially for nestedness *)
    <|> (fun x ->
        (*Format.printf "@.Nested->@.";*)
        nested_delimiters_parser generate_outer_delimiter_parsers x)
    (* whitespace is handled specially because we may change whether they are significant for matching. Only parse
       whitespace if it isn't a prefix required for alphanumeric delimiters, which take priority.*)
    <|> (fun x -> (*Format.printf "attempt <sp.>%!"; *) (spaces1 |>> generate_spaces_parser) x)
    (* everything else *)
    <|> (fun x ->
        ((many1 (is_not (reserved _s)) >>= fun cl ->
          if debug then Format.printf "<cl>%s</cl>" @@ String.of_char_list cl;
          return @@ String.of_char_list cl)
         |>> generate_string_token_parser) x)

  and generate_outer_delimiter_parsers ~left_delimiter ~right_delimiter s =
    let _is_alphanum _delim =
      Pcre.(pmatch ~rex:(regexp "^[0-9A-Za-z]+$") _delim) in
    let _whitespace = many1 space |>> String.of_char_list in
    let _required_delimiter_terminal =
      many1 (is_not alphanum) >>= fun x -> return @@ String.of_char_list x
    in
    (generate_parsers s >>= fun p_list ->
     (turn_holes_into_matchers_for_this_level ~left_delimiter ~right_delimiter
        ([
          (fun s ->
             (* modification incoming: generate parser only if alphanum condition holds *)
             (
               (* this logic is needed for cases where we say 'def :[1] end' in the template,
                  and don't match partially on 'adef body endq' in the underlying generated
                  parser *)
               let _prev = prev_char s in
               let _curr = read_char s in
               let _next = next_char s in
               let print_if = function
                 | Some s -> s
                 | None -> '?'
               in
               if debug_hole then Format.printf "prev: %c curr: %c next: %c@."
                   (print_if _prev)
                   (print_if _curr)
                   (print_if _next);
               (if _is_alphanum left_delimiter then
                  (if Option.is_some _prev && _is_alphanum (Char.to_string (Option.value_exn _prev)) then
                     fail "no"
                   else
                     (* why don't i check for after part here? dunno *)
                     string left_delimiter)
                else
                  string left_delimiter
               )
               >>= fun _ -> f [left_delimiter]) s)]
          @ p_list
          @ [
            (* fixes the case for 'echo '(def body endly)' | ./comby 'def :[1] end' ':[1]' .rb -stdin' *)
            (if _is_alphanum right_delimiter then
               string right_delimiter >>= fun delim ->
               look_ahead @@ (eof <|> skip (_required_delimiter_terminal)) >>= fun _ ->
               return delim
             else
               string right_delimiter)
            >>= fun _ -> f [right_delimiter]])
      |> sequence_chain)
     |> return
    ) s

  let general_parser_generator s =
    let outer_p =
      generate_parsers s >>= fun p_list ->
      (* eof of template is here *)
      eof >> (* result is unit so ignore *)
      (* customize the inner parser *)
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
               (* respect grammar but ignore contents up to a match *)
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
    (* Use a match type for state so we can reuse parsers for inner and outer *)
    match parse_string general_parser_generator template (Match.create ()) with
    | Success p -> Ok p
    | Failed (msg, _) -> Or_error.error_string msg

  (** shift: start the scan in the source at an offset *)
  let first' shift p source : Match.t Or_error.t =
    let set_start_pos p = fun s -> p (advance_state s shift) in
    let p = set_start_pos p in
    match parse_string' p source (Match.create ()) with
    | Success (_, result) -> Ok result
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
      if original_source = "" || template = "" then
        return []
      else
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
