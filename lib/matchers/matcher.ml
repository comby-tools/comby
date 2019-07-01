open Core
open MParser

open Configuration
open Match
open Range
open Location
open Types

let configuration_ref = ref (Configuration.create ())
let weaken_delimiter_hole_matching = false

let debug = false

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

  let nested_delimiters_parser (f : 'a nested_delimiter_callback) =
    Syntax.user_defined_delimiters
    |> List.map ~f:(fun (left_delimiter, right_delimiter) ->
        Parsers.Delimiters.between
          (f ~left_delimiter ~right_delimiter)
          left_delimiter right_delimiter)
    |> choice

  (** All code can have comments interpolated *)
  let generate_string_token_parser str : ('c, _) parser =
    many comment_parser
    >> string str
    >> many comment_parser
    >>= fun result -> f result

  let greedy_hole_parser () =
    string ":[" >> (many (alphanum <|> char '_') |>> String.of_char_list) << string "]"

  let non_space_hole_parser () =
    string ":[" >> (many (alphanum <|> char '_') |>> String.of_char_list) << string ".]"

  let line_hole_parser () =
    string ":[" >> (many (alphanum <|> char '_') |>> String.of_char_list) << string "\\n]"

  let blank_hole_parser () =
    string ":[" >> (many1 blank) >> (many (alphanum <|> char '_') |>> String.of_char_list) << string "]"

  let single_hole_parser () =
    string ":[[" >> (many (alphanum <|> char '_') |>> String.of_char_list) << string "]]"
    >>= fun id ->
    return (id, [], None)

  let reserved_delimiters =
    let reserved_delimiters =
      List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) -> [from; until])
      |> List.map ~f:(Fn.compose skip string)
    in
    let reserved_escapable_strings =
      List.concat_map Syntax.escapable_string_literals ~f:(fun x -> [x])
      |> List.map ~f:(Fn.compose skip string)
    in
    let reserved_raw_strings =
      List.concat_map Syntax.raw_string_literals ~f:(fun (from, until) -> [from; until])
      |> List.map ~f:(Fn.compose skip string)
    in
    let single =
      skip @@ single_hole_parser ()
    in
    let greedy =
      skip @@ greedy_hole_parser ()
    in
    let non_space =
      skip @@ non_space_hole_parser ()
    in
    let blank =
      skip @@ blank_hole_parser ()
    in
    let line =
      skip @@ line_hole_parser ()
    in
    [non_space; line; blank] @
    [single] @
    [greedy]
    @ reserved_delimiters @ reserved_escapable_strings @ reserved_raw_strings
    (* attempt the reserved: otherwise, if something passes partially,
       it won't detect that single or greedy is reserved *)
    |> List.map ~f:attempt
    |> choice

  let reserved =
    reserved_delimiters
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
    let between_nested_delims p from =
      let until = until_of_from from in
      between (string from) (string until) p
      |>> fun result -> (String.concat @@ [from] @ result @ [until])
    in
    let between_nested_delims p =
      let trigger_nested_parsing_prefix =
        if weaken_delimiter_hole_matching then
          match left_delimiter, right_delimiter with
          | Some left_delimiter, Some right_delimiter ->
            [ (left_delimiter, right_delimiter) ]
          | _ -> Syntax.user_defined_delimiters
        else
          Syntax.user_defined_delimiters
      in
      trigger_nested_parsing_prefix
      |> List.map ~f:fst
      |> List.map ~f:(between_nested_delims p)
      |> choice
    in
    (* applies looser delimiter constraints for matching *)
    let reserved =
      let trigger_nested_parsing_prefix =
        if weaken_delimiter_hole_matching then
          match left_delimiter, right_delimiter with
          | Some left_delimiter, Some right_delimiter ->
            [ (left_delimiter, right_delimiter) ]
          | _ -> Syntax.user_defined_delimiters
        else
          Syntax.user_defined_delimiters
      in
      trigger_nested_parsing_prefix
      |> List.concat_map ~f:(fun (from, until) -> [from; until])
      |> List.map ~f:string
      |> choice
    in
    (* a parser that understands the hole matching cut off points happen at
       delimiters *)
    let rec nested_grammar s =
      (comment_parser
       <|> raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
       <|> escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
       <|> delimsx
       <|> (is_not reserved |>> String.of_char))
        s
    and delimsx s = (between_nested_delims (many nested_grammar)) s
    in
    nested_grammar

  let turn_holes_into_matchers_for_this_level ?left_delimiter ?right_delimiter p_list =
    List.fold_right p_list ~init:[] ~f:(fun p acc ->
        let process_hole =
          match parse_string p "_signal_hole" (Match.create ()) with
          | Failed _ -> p
          | Success result ->
            match result with
            | Hole Alphanum (identifier, _, _, _) ->
              let rest =
                match acc with
                | [] -> eof >>= fun () -> f [""]
                | _ -> sequence_chain acc
              in
              let allowed =
                choice [alphanum; char '_']
                |>> String.of_char
              in
              (* if we collapse the not_followed_by part, we will disallow substring matching. *)
              let hole_semantics = many1 (not_followed_by rest "" >> allowed) in
              record_matches identifier hole_semantics

            | Hole Non_space (identifier, _dimension) ->
              let rest =
                match acc with
                | [] -> eof >>= fun () -> f [""]
                | _ -> sequence_chain acc
              in
              (* if we collapse the not_followed_by part, we will disallow substring matching. *)
              let allowed = non_space |>> String.of_char in
              let hole_semantics = many1 (not_followed_by rest "" >> allowed) in
              record_matches identifier hole_semantics

            | Hole Line (identifier, _dimension) ->
              let rest =
                match acc with
                | [] -> eof >>= fun () -> f [""]
                | _ -> sequence_chain acc
              in
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
              let rest =
                match acc with
                | [] -> eof >>= fun () -> f [""]
                | _ -> sequence_chain acc
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
      greedy_hole_parser () |>> fun id ->
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
      single_hole_parser () |>> fun (id, including, until_char) ->
      skip_signal (Hole (Alphanum (id, including, until_char, dimension)))

  let generate_hole_for_literal sort ~contents ~left_delimiter ~right_delimiter s =
    let p =
      many
        (attempt (hole_parser `Everything sort)
         <|> attempt (hole_parser `Alphanum sort)
         <|> ((many1 (is_not (string ":[" <|> string ":[["))
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
    (attempt (hole_parser `Non_space Code))
    <|> (attempt (hole_parser `Line Code))
    <|> (attempt (hole_parser `Blank Code))
    <|> (attempt (hole_parser `Everything Code))
    <|> (attempt (hole_parser `Alphanum Code))
    (* string literals are handled specially because match semantics change inside string delimiters *)
    <|> (raw_string_literal_parser (generate_hole_for_literal Raw_string_literal))
    <|> (escapable_string_literal_parser (generate_hole_for_literal Escapable_string_literal))
    (* whitespace is handled specially because we may change whether they are significant for matching *)
    <|> (spaces1 |>> generate_spaces_parser)
    (* nested delimiters are handled specially for nestedness *)
    <|> (nested_delimiters_parser generate_outer_delimiter_parsers)
    (* everything else *)
    <|> ((many1 (is_not reserved) |>> String.of_char_list) |>> generate_string_token_parser)

  and generate_outer_delimiter_parsers ~left_delimiter ~right_delimiter s =
    (generate_parsers s >>= fun p_list ->
     (turn_holes_into_matchers_for_this_level ~left_delimiter ~right_delimiter
        ([ string left_delimiter
           >>= fun _ -> f [left_delimiter]]
         @ p_list
         @ [ string right_delimiter
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
    match parse_string general_parser_generator template 0 with
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
