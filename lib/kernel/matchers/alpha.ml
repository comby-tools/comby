open Core_kernel
open MParser
open MParser_PCRE
open Configuration
open Match
open Range
open Location
module R = MakeRegexp (Regexp)

let configuration_ref = ref (Configuration.create ())
let weaken_delimiter_hole_matching = false

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

let debug_hole =
  match Sys.getenv "DEBUG_COMBY_HOLE" with
  | exception Not_found -> false
  | _ -> true

let debug_position =
  match Sys.getenv "DEBUG_COMBY_POS" with
  | exception Not_found -> false
  | _ -> true

let f _ = return Types.Unit

let extract_matched_text source { offset = match_start; _ } { offset = match_end; _ } =
  String.slice source match_start match_end

let is_not p s =
  if is_ok (p s) then
    Empty_failed (unknown_error s)
  else (
    match read_char s with
    | Some c -> Consumed_ok (c, advance_state s 1, No_error)
    | None -> Empty_failed (unknown_error s))

type 'a literal_parser_callback =
  contents:string -> left_delimiter:string -> right_delimiter:string -> 'a

type 'a nested_delimiter_callback = left_delimiter:string -> right_delimiter:string -> 'a

module Make (Lang : Types.Language.S) (Meta : Types.Metasyntax.S) (Ext : Types.External.S) = struct
  module rec Matcher : Types.Matcher.S = struct
    module Syntax = Lang.Syntax
    include Lang.Info

    let wildcard = "_"

    let create v =
      Types.Ast.Template [ Hole { variable = v; pattern = v; offset = 0; kind = Value } ]

    let implicit_equals_satisfied environment identifier range matched =
      if debug then Format.printf "Looking up %s@." identifier;
      match Environment.lookup environment identifier with
      | None -> Some (Environment.add ~range environment identifier (String.concat matched))
      | Some _ when String.(identifier = wildcard) -> Some environment
      | Some existing_value when String.(existing_value = String.concat matched) ->
        let identifier' = Format.sprintf "%s_equal_%s" identifier (!configuration_ref.fresh ()) in
        let environment' = Environment.add ~range environment identifier' (String.concat matched) in
        Some environment'
      | _ -> None

    let escapable_string_literal_parser (f : 'a literal_parser_callback) =
      (match Syntax.escapable_string_literals with
       | None -> []
       | Some { delimiters; escape_character } ->
         List.map delimiters ~f:(fun delimiter ->
           let module M =
             Parsers.String_literals.Alpha.Escapable.Make (struct
               let delimiter = delimiter
               let escape = escape_character
             end)
           in
           M.base_string_literal
           >>= fun contents ->
           return (f ~contents ~left_delimiter:delimiter ~right_delimiter:delimiter)))
      |> choice

    let raw_string_literal_parser (f : 'a literal_parser_callback) =
      List.map Syntax.raw_string_literals ~f:(fun (left_delimiter, right_delimiter) ->
        let module M =
          Parsers.String_literals.Alpha.Raw.Make (struct
            let left_delimiter = left_delimiter
            let right_delimiter = right_delimiter
          end)
        in
        M.base_string_literal
        >>= fun contents -> return (f ~contents ~left_delimiter ~right_delimiter))
      |> choice

    let comment_parser =
      match Syntax.comments with
      | [] -> MParser.zero
      | syntax ->
        List.map syntax ~f:(function
          | Multiline (left, right) ->
            let module M =
              Parsers.Comments.Alpha.Multiline.Make (struct
                let left = left
                let right = right
              end)
            in
            M.comment
          | Nested_multiline (left, right) ->
            let module M =
              Parsers.Comments.Alpha.Nested_multiline.Make (struct
                let left = left
                let right = right
              end)
            in
            M.comment
          | Until_newline start ->
            let module M =
              Parsers.Comments.Alpha.Until_newline.Make (struct
                let start = start
              end)
            in
            M.comment)
        |> choice

    let escapable_literal_grammar ~right_delimiter =
      match Syntax.escapable_string_literals with
      | None -> zero
      | Some { escape_character; _ } ->
        string (Format.sprintf "%c%s" escape_character right_delimiter)
        <|> string (Format.sprintf "%c%c" escape_character escape_character)
        <|> (is_not (string right_delimiter) |>> String.of_char)

    let raw_literal_grammar ~right_delimiter = is_not (string right_delimiter) |>> String.of_char

    (* Does not take into account code comments. *)
    let generate_pure_spaces_parser () = spaces1 >>= fun result -> f result

    (* Takes into account code comments. *)
    let generate_spaces_parser () =
      many1 @@ choice [ skip comment_parser; spaces1 ] >>= fun _ -> f Types.Unit

    let sequence_chain (plist : ('c, Match.t) parser sexp_list) : ('c, Match.t) parser =
      List.fold plist ~init:(return Types.Unit) ~f:( >> )

    let with_debug_matcher s tag =
      if debug then (
        match tag with
        | `Position tag ->
          if debug_position then (
            let prev = prev_char s in
            let curr = read_char s in
            let next = next_char s in
            let print_if = function
              | Some s -> s
              | None -> '?'
            in
            Format.printf "Position Tag: %s@." tag;
            Format.printf
              "H_prev: %c H_curr: %c H_next: %c@."
              (print_if prev)
              (print_if curr)
              (print_if next))
        | `Delimited delimited -> Format.printf "<d>%s</d>%!" delimited
        | `Delimited_suffix suffix -> Format.printf "<d_s>%s</d_s>%!" suffix
        | `Checkpoint (label, s) -> Format.printf "Point(%s):<d>%s</d>" label s
        | _ -> assert false)

    let is_alphanum delim = Pcre.(pmatch ~rex:(regexp "^[[:alnum:]]+$") delim)
    let whitespace : (string, Match.t) parser = many1 space |>> String.of_char_list
    let not_alphanum = many1 (is_not alphanum) |>> String.of_char_list

    let reserved_alphanum_delimiter_must_satisfy =
      Syntax.user_defined_delimiters
      |> List.filter_map ~f:(fun (from, until) ->
           if not (is_alphanum from) then
             Some [ from; until ]
           else
             None)
      |> List.concat
      |> List.map ~f:string
      |> List.map ~f:attempt

    let nested_delimiters_parser (f : 'a nested_delimiter_callback) =
      (* All alphanum delimiter fixups happen in the generated parser, not here. *)
      let between p from until s =
        (string from
        >>= fun from ->
        if debug then with_debug_matcher s (`Delimited from);
        p
        >>= fun p_result ->
        string until
        >>= fun until ->
        if debug then with_debug_matcher s (`Delimited until);
        return p_result)
          s
      in
      Syntax.user_defined_delimiters
      |> List.map ~f:(fun (left_delimiter, right_delimiter) ->
           between (f ~left_delimiter ~right_delimiter) left_delimiter right_delimiter)
      |> choice
      (* Backtrack on failure, specifically for alphanum. *)
      |> attempt

    (** Generates a word that is NOT OK with having alphanum chars before or after
        it. This disables substring parsing. *)
    let generate_word chars : ('c, _) parser =
     fun s ->
      let prev = prev_char s in
      (match prev with
       | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
       | _ ->
         string (String.of_char_list chars)
         >>= fun result ->
         (* there has to be at least one "not alphanum" after this parser for it to succeed, or eof. *)
         look_ahead (skip (many1 (is_not alphanum)) <|> eof) >>= fun _ -> f result)
        s

    let generate_string_token_parser str : ('c, _) parser =
      many comment_parser >> string str >>= fun result -> f result

    let identifier () = choice @@ List.map ~f:char (String.to_list Meta.identifier)
    let optional_identifier () = many (identifier ()) |>> String.of_char_list
    let identifier () = many1 (identifier ()) |>> String.of_char_list
    let hole_body () = identifier ()

    let regex_body separator suffix () =
      let rec expr s =
        (choice
           [ (char '['
             >> many1 expr
             << char ']'
             |>> fun char_class -> Format.sprintf "[%s]" @@ String.concat char_class)
           ; (char '\\' >> any_char |>> fun c -> Format.sprintf "\\%c" c)
           ; is_not (string suffix) |>> Char.to_string
           ])
          s
      in
      let regex_identifier () =
        optional_identifier ()
        >>= fun v ->
        char separator
        >> many1 expr
        >>= fun e -> return (Format.sprintf "%s%c%s" v separator (String.concat e))
      in
      regex_identifier ()
      >>= fun identifier ->
      if debug then Format.printf "Regex accepts %s@." identifier;
      return identifier

    let p = function
      | Some delim -> skip (string delim)
      | None -> return ()

    let hole_parsers =
      List.fold ~init:[] Meta.syntax ~f:(fun acc -> function
        | Hole (sort, Delimited (left, right)) -> (sort, p left >> hole_body () << p right) :: acc
        | Hole (sort, Reserved_identifiers l) -> (sort, choice (List.map ~f:string l)) :: acc
        | Regex (left, separator, right) ->
          (Regex, p (Some left) >> regex_body separator right () << p (Some right)) :: acc)

    let reserved_holes = List.map hole_parsers ~f:(fun (_, parser) -> parser >>= fun _ -> return "")

    let reserved_parsers () =
      (* Alphanum blocks *)
      let required_from_suffix = not_alphanum in
      let required_until_suffix = not_alphanum in
      let handle_alphanum_delimiters_reserved_trigger from until =
        let from_parser s =
          (match prev_char s with
           | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
           | _ ->
             string from >>= fun from -> look_ahead required_from_suffix >>= fun _ -> return from)
            s
        in
        let until_parser s =
          (string until
          >>= fun until ->
          eof
          <|> look_ahead (skip required_until_suffix)
          >>= fun _ ->
          (* if current char/next_char is alphanum, make unsat. *)
          let prev = prev_char s in
          if debug then with_debug_matcher s (`Position "reserved_delimiter_until");
          match prev with
          | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
          | _ -> return until)
            s
        in
        [ from_parser; until_parser ]
      in
      (* Isomorphic to Omega *)
      let user_defined_reserved_delimiters =
        List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) ->
          if is_alphanum from && is_alphanum until then
            handle_alphanum_delimiters_reserved_trigger from until
          else
            [ string from; string until ])
      in
      let user_defined_reserved_escapable_strings =
        match Syntax.escapable_string_literals with
        | Some { delimiters; _ } ->
          List.concat_map delimiters ~f:(fun delimiter -> [ string delimiter ])
        | None -> []
      in
      let user_defined_reserved_raw_strings =
        List.concat_map Syntax.raw_string_literals ~f:(fun (from, until) ->
          [ string from; string until ])
      in
      let user_defined_reserved_comments =
        List.concat_map Syntax.comments ~f:(function
          | Multiline (left, right) -> [ string left; string right ]
          | Nested_multiline (left, right) -> [ string left; string right ]
          | Until_newline start -> [ string start ])
      in
      [ user_defined_reserved_delimiters
      ; reserved_holes
      ; user_defined_reserved_escapable_strings
      ; user_defined_reserved_raw_strings
      ; user_defined_reserved_comments
        (* only needed once it's significant for matching and not treated like spaces*)
      ]
      |> List.concat
      |> List.map ~f:skip
      (* Attempt the reserved: otherwise, if a parser partially succeeds,
         it won't detect that single or greedy is reserved. *)
      |> List.map ~f:attempt
      |> choice

    let until_of_from from =
      Syntax.user_defined_delimiters
      |> List.find_map ~f:(fun (from', until) ->
           if String.equal from from' then Some until else None)
      |> function
      | Some until -> until
      | None -> assert false

    let record_matches identifier p : ('c, Match.t) parser =
      get_pos
      >>= fun (pre_index, pre_line, pre_column) ->
      p
      >>= fun matched ->
      get_pos
      >>= fun (post_index, post_line, post_column) ->
      let post_index, post_line, post_column =
        if String.(concat matched = "") then
          pre_index, pre_line, pre_column
        else
          post_index, post_line, post_column
      in
      get_user_state
      >>= fun { environment; _ } ->
      let pre_location : Location.t =
        Location.{ offset = pre_index; line = pre_line; column = pre_column }
      in
      let post_location : Location.t =
        Location.{ offset = post_index; line = post_line; column = post_column }
      in
      let range = { match_start = pre_location; match_end = post_location } in
      match implicit_equals_satisfied environment identifier range matched with
      | None -> fail "don't record, unsat"
      | Some environment ->
        update_user_state (fun result ->
          if debug then (
            Format.printf "Updating user state:@.";
            Format.printf "%s |-> %s@." identifier (String.concat matched);
            Format.printf
              "ID %s: %d:%d:%d - %d:%d:%d@."
              identifier
              pre_index
              pre_line
              pre_column
              post_index
              post_line
              post_column);
          { result with environment })
        >>= fun () -> f matched

    let alphanum_delimiter_must_satisfy =
      many1 (is_not (skip (choice reserved_alphanum_delimiter_must_satisfy) <|> skip alphanum))
      |>> String.of_char_list

    let generate_delimited_hole_parser
      ?priority_left_delimiter:left_delimiter
      ?priority_right_delimiter:right_delimiter
      =
      let delimiters =
        if weaken_delimiter_hole_matching then (
          match left_delimiter, right_delimiter with
          | Some left_delimiter, Some right_delimiter -> [ left_delimiter, right_delimiter ]
          | _ -> Syntax.user_defined_delimiters)
        else
          Syntax.user_defined_delimiters
      in
      let reserved =
        List.concat_map delimiters ~f:(fun (from, until) -> [ string from; string until ])
        |> List.map ~f:attempt
        |> choice
      in
      let other = is_not reserved |>> String.of_char in
      (* A parser that understands the hole matching cut off points happen at
         delimiters. *)
      let rec nested_grammar s =
        (comment_parser
        <|> raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ ->
              contents)
        <|> escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ ->
              contents)
        <|> (many1 space |>> String.of_char_list)
        <|> attempt @@ delims_over_holes
        (* Only consume if not reserved. If it is reserved, we want to trigger the 'many'
            in (many nested_grammar) to continue. *)
        <|> other)
          s
      and delims_over_holes s =
        let between_nested_delims p =
          let capture_delimiter_result p ~from =
            let until = until_of_from from in
            between (string from) (string until) p
            >>= fun result -> return (String.concat @@ [ from ] @ result @ [ until ])
          in
          delimiters
          |> List.map ~f:(fun pair -> capture_delimiter_result p ~from:(fst pair))
          |> choice
        in
        (between_nested_delims (many nested_grammar)) s
      in
      delims_over_holes

    let generate_everything_hole_parser
      ?priority_left_delimiter:left_delimiter
      ?priority_right_delimiter:right_delimiter
      ?at_depth
      =
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
        if weaken_delimiter_hole_matching then (
          match left_delimiter, right_delimiter with
          | Some left_delimiter, Some right_delimiter -> [ left_delimiter, right_delimiter ]
          | _ -> Syntax.user_defined_delimiters)
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
          string from
          >>= fun open_delimiter ->
          with_debug_hole
            (`Checkpoint ("open_delimiter_<pre>" ^ prefix ^ "</pre>_sat_for", open_delimiter))
          >>= fun _ ->
          (* Use look_ahead to ensure that there is, e.g., whitespace after this
             possible delimiter, but without consuming input. Whitespace needs to
             not be consumed so that we can detect subsequent delimiters. *)
          look_ahead mandatory_suffix
          >>= fun suffix ->
          with_debug_hole
            (`Checkpoint ("open_delimiter_<suf>" ^ suffix ^ "</suf>_sat_for", open_delimiter))
          >>= fun _ -> return (prefix, open_delimiter)
        in
        let satisfy_closing_delimiter =
          string until
          >>= fun close_delimiter ->
          look_ahead @@ mandatory_suffix
          >>= fun suffix ->
          with_debug_hole
            (`Checkpoint ("close_delimiter_<suf>" ^ suffix ^ "</suf>_sat_for", close_delimiter))
          >>= fun _ -> return close_delimiter
        in
        (fun s ->
          let prev = prev_char s in
          (satisfy_opening_delimiter prev
          >>= fun (prefix, open_delimiter) ->
          p
          >>= fun in_between ->
          with_debug_hole (`Body in_between)
          >>= fun in_between ->
          satisfy_closing_delimiter
          >>= fun close_delimiter ->
          return ((prefix ^ open_delimiter) ^ in_between ^ close_delimiter))
            s)
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
        List.map [ from; until ] ~f:(fun delimiter s ->
          let prev = prev_char s in
          (match prev with
           | Some prev when is_alphanum (Char.to_string prev) ->
             (* If prev is alphanum, this can't possibly be a reserved delimiter. Just continue. *)
             fail "unsat"
           | _ -> string delimiter >>= fun _ -> look_ahead required_delimiter_terminal)
            s)
      in
      (* The cases for which we need to stop parsing just characters
         and consider delimiters. *)
      let reserved =
        List.concat_map delimiters ~f:(fun (from, until) ->
          if is_alphanum from then
            handle_alphanum_delimiters_reserved_trigger from until
          else
            [ string from; string until ])
        |> List.map ~f:attempt
        |> choice
      in
      (* A parser that understands the hole matching cut off points happen at
         delimiters. *)
      let rec nested_grammar s =
        (comment_parser
        <|> raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ ->
              contents)
        <|> escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ ->
              contents)
        <|> (many1 (if Option.equal Int.equal (Some 0) at_depth then blank else space)
            >>= fun r -> with_debug_hole (`Spaces r))
        <|> (attempt @@ delims_over_holes >>= fun r -> with_debug_hole (`Delimited r))
        (* Only consume if not reserved. If it is reserved, we want to trigger the 'many'
            in (many nested_grammar) to continue. *)
        <|> (is_not (reserved <|> (space |>> Char.to_string))
            >>= fun r -> with_debug_hole (`Character r)))
          s
      and delims_over_holes s =
        let between_nested_delims p =
          let capture_delimiter_result p ~from =
            let until = until_of_from from in
            if is_alphanum from then
              handle_alphanum_delimiters from until p
            else
              between (string from) (string until) p
              >>= fun result -> return (String.concat @@ [ from ] @ result @ [ until ])
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
          ( parse_string p " " (Match.create ())
          , (* suffix parser could be a hole. It needs to fail on
             parsing something like "X" to be a whitespace parser *)
            parse_string p "X" (Match.create ()) )
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
      match List.nth (List.rev p_list) (i + 1) with
      | Some p -> p
      | None ->
        if debug then Format.printf "Prefix parser unsat@.";
        fail "unsat"

    let turn_holes_into_matchers_for_this_level
      ?left_delimiter
      ?right_delimiter
      (p_list : (Types.production, Match.t) parser list)
      : (Types.production, Match.t) parser list
      =
      List.fold (List.rev p_list) ~init:[] ~f:(fun acc p ->
        match parse_string p "_signal_hole" (Match.create ()) with
        | Failed _ -> p :: acc
        | Success (Types.Hole { sort; identifier; dimension; at_depth }) ->
          (match sort with
           | Regex ->
             let separator =
               List.find_map_exn Meta.syntax ~f:(function
                 | Hole _ -> None
                 | Regex (_, separator, _) -> Some separator)
             in
             let identifier, pattern = String.lsplit2_exn identifier ~on:separator in
             (* FIXME parse *)
             let identifier = if String.(identifier = "") then wildcard else identifier in
             if debug then Format.printf "Regex: Id: %s Pat: %s@." identifier pattern;
             let compiled_regexp = R.make_regexp pattern in
             let regexp_parser = R.regexp compiled_regexp in
             let base_parser = [ regexp_parser ] in
             let base_parser =
               (* adds begin line parser if the pattern has ^ anchor *)
               if String.is_prefix pattern ~prefix:"^" then (
                 let p = R.make_regexp (String.drop_prefix pattern 1) |> R.regexp in
                 (char '\n' >>= fun _ -> p) :: base_parser)
               else
                 base_parser
             in
             let base_parser =
               (* adds end line parser if the pattern has $ anchor *)
               if String.is_suffix pattern ~suffix:"$" then (
                 let p = R.make_regexp (String.drop_suffix pattern 1) |> R.regexp in
                 (p << (skip @@ char '\n' <|> eof)) :: base_parser)
               else
                 base_parser
             in
             let hole_semantics =
               choice base_parser
               >>= fun result ->
               if debug then Format.printf "Regex success: %s@." result;
               return [ result ]
             in
             record_matches identifier hole_semantics :: acc
           | Alphanum ->
             let allowed = choice [ alphanum; char '_' ] |>> String.of_char in
             let hole_semantics = many1 allowed in
             record_matches identifier hole_semantics :: acc
           | Non_space ->
             let allowed =
               [ skip space; reserved_parsers () ] |> choice |> is_not |>> Char.to_string
             in
             let rest =
               match acc with
               | [] -> eof >>= fun () -> f [ "" ]
               | _ -> sequence_chain acc
             in
             let hole_semantics = many1 (not_followed_by rest "" >> allowed) in
             record_matches identifier hole_semantics :: acc
           | Line ->
             let allowed =
               many (is_not (char '\n')) |>> fun x -> [ String.of_char_list x ^ "\n" ]
             in
             let hole_semantics = allowed << char '\n' in
             record_matches identifier hole_semantics :: acc
           | Blank ->
             let allowed = blank |>> String.of_char in
             let hole_semantics = many1 allowed in
             record_matches identifier hole_semantics :: acc
           | Expression ->
             let non_space =
               [ skip space; reserved_parsers () ] |> choice |> is_not |>> Char.to_string
             in
             let delimited =
               generate_delimited_hole_parser
                 ?priority_left_delimiter:left_delimiter
                 ?priority_right_delimiter:right_delimiter
             in
             let matcher = non_space <|> delimited in
             let rest =
               match acc with
               | [] -> eof >>= fun () -> f [ "" ]
               | _ -> sequence_chain acc
             in
             let hole_semantics = many1 (not_followed_by rest "" >> matcher) in
             record_matches identifier hole_semantics :: acc
           | Everything ->
             let matcher =
               match dimension with
               | Code ->
                 generate_everything_hole_parser
                   ?priority_left_delimiter:left_delimiter
                   ?priority_right_delimiter:right_delimiter
                   ?at_depth
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
               | [] -> eof >>= fun () -> f [ "" ]
               | _ -> sequence_chain acc
             in
             (* Continue until rest, but don't consume rest. acc will
                   propagate the rest that needs to be consumed. *)
             let hole_semantics = many (not_followed_by rest "" >> matcher) in
             record_matches identifier hole_semantics :: acc)
        | Success Unit -> acc (* for comment *)
        | Success _ -> failwith "Hole expected")

    let hole_parser ?at_depth sort dimension =
      let open Types.Hole in
      let hole_parser =
        let open Polymorphic_compare in
        List.fold ~init:[] hole_parsers ~f:(fun acc (sort', parser) ->
          if sort' = sort then parser :: acc else acc)
      in
      let skip_signal hole = skip (string "_signal_hole") |>> fun () -> Types.Hole hole in
      let at_depth =
        if !configuration_ref.match_newline_toplevel then
          None
        else (
          (* Match newlines at toplevel if for languages that are likely to match
             around context-sensitive tags *)
          match Lang.Info.name with
          | "HTML" | "XML" | "Text" | "LaTeX" -> None
          | _ -> at_depth)
      in
      match hole_parser with
      | [] -> fail "none" (* not defined *)
      | l ->
        choice l
        |>> (function
        | identifier -> skip_signal { sort; identifier; dimension; at_depth })

    let generate_hole_for_literal dimension ~contents ~left_delimiter ~right_delimiter s =
      let holes =
        choice @@ List.map hole_parsers ~f:(fun (kind, _) -> attempt (hole_parser kind dimension))
      in
      let reserved_holes = reserved_holes |> List.map ~f:skip |> List.map ~f:attempt |> choice in
      let p =
        many
        @@ choice
             [ holes
             ; spaces1 |>> generate_pure_spaces_parser
             ; many1 (is_not (choice [ reserved_holes; skip space ]))
               |>> String.of_char_list
               |>> generate_string_token_parser
             ]
      in
      match parse_string p contents (Match.create ()) with
      | Success p ->
        (match dimension with
         | Escapable_string_literal | Raw_string_literal ->
           (turn_holes_into_matchers_for_this_level ~left_delimiter ~right_delimiter p
           |> sequence_chain)
             s
         | _ -> assert false)
      | Failed (_msg, _) -> failwith "literal parser did not succeed"

    let depth = ref (-1)

    let rec generate_parsers s =
      depth := !depth + 1;
      many (common s)

    and common _s =
      let holes at_depth =
        hole_parsers |> List.map ~f:(fun (sort, _) -> attempt (hole_parser sort Code ~at_depth))
      in
      choice
        [ (choice (holes !depth)
          >>= fun result ->
          if debug then Format.printf "Depth hole %d@." !depth;
          return result)
          (* String literals are handled specially because match semantics change inside string delimiters. *)
        ; raw_string_literal_parser (generate_hole_for_literal Raw_string_literal)
        ; escapable_string_literal_parser (generate_hole_for_literal Escapable_string_literal)
          (* Nested delimiters are handled specially for nestedness. *)
        ; (nested_delimiters_parser generate_outer_delimiter_parsers
          >>= fun result ->
          depth := !depth - 1;
          return result)
          (* Skip comments in the template, just succeed. If desired, could return the comment string. *)
        ; (many1 (skip comment_parser <|> spaces1) |>> fun _ -> generate_spaces_parser ())
          (* Optional: parse identifiers and disallow substring matching *)
        ; (if !configuration_ref.disable_substring_matching then
             many1 (alphanum <|> char '_') |>> generate_word
          else
            zero (* Everything else. *))
        ; many1 @@ is_not @@ choice [ reserved_parsers (); skip space ]
          |>> (fun cl ->
                if debug then Format.printf "<cl>%s</cl>" @@ String.of_char_list cl;
                String.of_char_list cl)
          |>> generate_string_token_parser
        ]

    and generate_outer_delimiter_parsers ~left_delimiter ~right_delimiter s =
      let before s =
        (if debug_hole then with_debug_matcher s (`Position "generate_outer_delimiter");
         let p =
           if is_alphanum left_delimiter then (
             (* This logic is needed for cases where we say 'def :[1] end' in the template,
                 and don't match partially on, say, 'adef body endq' in the underlying generated
                 parser. *)
             let prev = prev_char s in
             match prev with
             | Some prev when is_alphanum (Char.to_string prev) -> fail "unsat"
             | _ -> string left_delimiter)
           else
             string left_delimiter
         in
         p >>= fun _ -> f [ left_delimiter ])
          s
      in
      let after =
        let p =
          if is_alphanum right_delimiter then
            (* This handles the case for something like 'def body endly'. *)
            string right_delimiter
            >>= fun delim -> look_ahead @@ (eof <|> skip not_alphanum) >>= fun _ -> return delim
          else
            string right_delimiter
        in
        p >>= fun _ -> f [ right_delimiter ]
      in
      (generate_parsers s
      >>= fun p_list ->
      turn_holes_into_matchers_for_this_level
        ~left_delimiter
        ~right_delimiter
        ([ before ] @ p_list @ [ after ])
      |> sequence_chain
      |> return)
        s

    let general_parser_generator s =
      let outer_p =
        generate_parsers s
        >>= fun p_list ->
        (* EOF of template is here. *)
        eof
        >>
        (* Result is unit so ignore. *)
        (* Customize the inner parser. *)
        let inner_p =
          let matcher : ('a, Match.t) parser =
            turn_holes_into_matchers_for_this_level p_list |> sequence_chain
          in
          let matcher : ('a, Match.t) parser =
            let with_positions (matcher : ('a, Match.t) parser) : ('a, Match.t) parser =
              get_pos
              >>= fun (pre_offset, pre_line, pre_column) ->
              matcher
              >>= fun _last_production ->
              get_pos
              >>= fun (post_offset, post_line, post_column) ->
              let match_start = { offset = pre_offset; line = pre_line; column = pre_column } in
              let match_end = { offset = post_offset; line = post_line; column = post_column } in
              let range = { match_start; match_end } in
              update_user_state (fun result -> { result with range }) >> return Types.Unit
            in
            with_positions matcher
          in
          match !configuration_ref.match_kind with
          | Exact -> matcher << eof
          | Fuzzy ->
            many
              (not_followed_by matcher ""
              >> ((* Respect grammar but ignore contents up to a match. *)
                  skip comment_parser
                 <|> skip
                       (raw_string_literal_parser
                          (fun ~contents:_ ~left_delimiter:_ ~right_delimiter:_ -> ()))
                 <|> skip
                       (escapable_string_literal_parser
                          (fun ~contents:_ ~left_delimiter:_ ~right_delimiter:_ -> ()))
                 <|> skip any_char))
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
      if debug then Format.printf "First for shift %d@." shift;
      let set_start_pos p s = p (advance_state s shift) in
      let p = set_start_pos p in
      match parse_string (pair p get_user_state) source (Match.create ()) with
      | Success (_, result) ->
        if String.is_empty source then
          (* If source is empty and p succeeds, it's the trivial case. We set
             the result manually. *)
          Ok
            { result with
              range =
                { match_start = { offset = 0; line = 1; column = 1 }
                ; match_end = { offset = 0; line = 1; column = 1 }
                }
            }
        else
          Ok result
      | Failed (msg, _) -> Or_error.error_string msg

    let first ?configuration ?shift ?filepath template source =
      let open Or_error in
      let (_ : string option) = filepath in
      configuration_ref := Option.value configuration ~default:!configuration_ref;
      to_template template
      >>= fun p ->
      let shift =
        match shift with
        | Some s -> s
        | None -> 0
      in
      first' shift p source

    let all
      ?configuration
      ?filepath
      ?(rule = [ Types.Ast.True ])
      ~template
      ~source:original_source
      ()
      : Match.t list
      =
      let (_ : string option) = filepath in
      let Rule.{ nested; _ } = Rule.options rule in
      let template, rule = Preprocess.map_aliases (module Meta) (module Ext) template (Some rule) in
      let rule = Option.value_exn rule in
      (* OK in this case *)
      let rec aux_all ?configuration ?(nested = false) ~template ~source:original_source () =
        let open Or_error in
        depth := -1;
        configuration_ref := Option.value configuration ~default:!configuration_ref;
        let make_result = function
          | Ok ok -> ok
          | Error _ -> []
        in
        make_result
        @@ (to_template template
           >>= fun p ->
           let p =
             if String.is_empty template then
               MParser.(eof >> return Types.Unit)
             else
               p
           in
           let rec aux acc shift =
             match first' shift p original_source with
             | Ok ({ range = { match_start; match_end; _ }; environment; _ } as result) ->
               if debug then Format.printf "Ok first'";
               let shift = match_end.offset in
               let shift, matched =
                 if match_start.offset = match_end.offset then
                   ( match_start.offset + 1
                   , "" (* advance one if the matched content is the empty string *) )
                 else
                   shift, extract_matched_text original_source match_start match_end
               in
               if debug then Format.printf "Extracted matched: %s@." matched;
               let result = { result with matched } in
               let result =
                 if debug then Format.printf "Rule: %s@." (Sexp.to_string @@ Rule.sexp_of_t rule);
                 (* FIXME we should not have to convert here. Pass module, but after fixing this functor's signature. *)
                 let metasyntax =
                   Metasyntax.
                     { syntax = Meta.syntax; identifier = Meta.identifier; aliases = Meta.aliases }
                 in
                 let external_handler = Ext.handler in
                 let sat, env =
                   Program.apply
                     ~metasyntax
                     ~external_handler
                     ~substitute_in_place:!configuration_ref.substitute_in_place
                     ?filepath
                     rule
                     environment
                 in
                 if debug && Option.is_some env then
                   Format.printf
                     "Got back: %b %S"
                     sat
                     (Match.Environment.to_string @@ Option.value_exn env);
                 let new_env = if sat then env else None in
                 match new_env with
                 | None -> None
                 | Some env -> Some { result with environment = env }
               in
               if shift >= String.length original_source then (
                 match result with
                 | Some result -> result :: acc
                 | None -> acc)
               else (
                 match result with
                 | Some result -> aux (result :: acc) shift
                 | None -> aux acc shift)
             | Error _ -> acc
           in
           let matches = aux [] 0 |> List.rev in
           if nested then
             return (compute_nested_matches ?configuration ~nested template matches @ matches)
           else
             return matches)
      and compute_nested_matches ?configuration ?nested template matches =
        let rec aux acc matches =
          match (matches : Match.t list) with
          | [] -> acc
          | { environment; _ } :: rest ->
            List.fold ~init:acc (Environment.vars environment) ~f:(fun acc v ->
              let source_opt = Environment.lookup environment v in
              match source_opt with
              | Some source ->
                let nested_matches =
                  match first ?configuration template source with
                  | Ok { matched; _ } when String.(matched <> source) ->
                    if String.(matched = "") && String.length source > 1 then (
                      let matches = aux_all ?configuration ?nested ~template ~source () in
                      let { match_start = ms; _ } =
                        Option.value_exn (Environment.lookup_range environment v)
                      in
                      List.map matches ~f:(fun m ->
                        (*Format.eprintf "Doing %S. Parent matched is %S. Start is %d@." m.matched matched ms.offset;*)
                        let environment =
                          List.fold
                            (Environment.vars m.environment)
                            ~init:m.environment
                            ~f:(fun env var ->
                            let open Option in
                            let updated : environment option =
                              Environment.lookup_range env var
                              >>| fun r ->
                              let range =
                                { match_start =
                                    { r.match_start with
                                      offset = ms.offset + r.match_start.offset - 1
                                    }
                                ; match_end =
                                    { r.match_end with offset = ms.offset + r.match_end.offset - 1 }
                                }
                              in
                              Environment.update_range env var range
                            in
                            match updated with
                            | None -> env
                            | Some env -> env)
                        in
                        let range =
                          { match_start =
                              { m.range.match_start with
                                offset = ms.offset + m.range.match_start.offset - 1
                              }
                          ; match_end =
                              { m.range.match_end with
                                offset = ms.offset + m.range.match_end.offset - 1
                              }
                          }
                        in
                        { m with range; environment }))
                    else
                      []
                  | _ -> []
                in
                acc @ nested_matches
              | _ -> acc)
            @ aux acc rest
        in
        aux [] matches
      in
      if nested then (* Use sort on offset for a top-down ordering. *)
        aux_all ?configuration ~nested ~template ~source:original_source ()
        |> List.sort ~compare:(fun left right ->
             left.range.match_start.offset - right.range.match_start.offset)
      else
        (* Don't change list order for non-nested matches--it's backwards and matters for rewriting. *)
        aux_all ?configuration ~nested ~template ~source:original_source ()

    let set_rewrite_template _ = () (* Unused in alpha matcher *)
  end

  and Program : sig
    val apply
      :  ?substitute_in_place:bool
      -> ?metasyntax:Types.Metasyntax.t
      -> ?external_handler:Types.External.t
      -> ?filepath:string
      -> Rule.t
      -> Match.environment
      -> Evaluate.result
  end = struct
    let apply ?substitute_in_place ?metasyntax ?external_handler ?filepath rule env =
      let Rule.{ nested; _ } = Rule.options rule in
      let subrule = if nested then [ Types.Ast.True; Option "nested" ] else [ Types.Ast.True ] in
      Evaluate.apply
        ?substitute_in_place
        ?metasyntax
        ?external_handler
        ?filepath
        ~match_all:(Matcher.all ~rule:subrule)
        rule
        env
  end

  include Matcher
end
