open Core_kernel

open Vangstrom

open Omega_parser_helper

type omega_match_production =
  { offset : int
  ; identifier : string
  ; text : string
  }
[@@deriving yojson]

type production =
  | Unit
  | String of string
  | Template_string of string
  | Hole of Types.hole
  | Match of omega_match_production

let configuration_ref = ref (Configuration.create ())

let implicit_equals_match_satisfied : bool ref = ref true
let current_environment_ref : Match.Environment.t ref = ref (Match.Environment.create ())
let matches_ref : Match.t list ref = ref []
let source_ref : string ref = ref ""

let push_implicit_equals_match_satisfied : bool ref = ref true
let push_environment_ref : Match.Environment.t ref = ref (Match.Environment.create ())
let push_matches_ref : Match.t list ref = ref []
let push_source_ref : string ref = ref ""

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

let rewrite =
  match Sys.getenv "REWRITE" with
  | exception Not_found -> false
  | _ -> true

let actual = Buffer.create 10

let rewrite_template = ref ""

let substitute template env =
  let substitution_formats =
    [ ":[ ", "]"
    ; ":[", ".]"
    ; ":[", "\\n]"
    ; ":[[", "]]"
    ; ":[", "]"
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

module Make (Language : Types.Language.S) (Meta : Metasyntax.S) = struct
  module rec Matcher : Types.Matcher.S = struct
    include Language.Info

    module Template = Template.Make(Meta)

    let wildcard = "_"

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

    let create v =
      Types.Ast.Template [Hole { variable = v; pattern = v; offset = 0; kind = Value }]

    let implicit_equals_satisfied environment identifier range matched =
      let open Match in
      if debug then Format.printf "Looking up %s@." identifier;
      match Environment.lookup environment identifier with
      | None -> Some (Environment.add ~range environment identifier matched)
      | Some _ when String.(identifier = wildcard) -> Some environment
      | Some existing_value when String.(existing_value = matched) ->
        let identifier' = Format.sprintf "%s_equal_%s" identifier (!configuration_ref.fresh ()) in
        let environment' = Environment.add ~range environment identifier' matched in
        Some environment'
      | _ -> None

    let r acc production : (production * 'a) t =
      let open Match in
      let open Location in
      let open Range in
      let acc = f acc production in
      match production with
      | String s ->
        if debug then Format.printf "Saw String: %S@." s;
        return (Unit, acc)
      | Match { offset = pos_begin; identifier; text = content } ->
        begin
          if debug then Format.printf "Match: %S @@ %d for %s@." content pos_begin identifier;
          (* line/col values are placeholders and not accurate until processed in pipeline.ml *)
          let before = { offset = pos_begin; line = 1; column = pos_begin + 1 } in
          let pos_after_offset = pos_begin + String.length content in
          let after = { offset = pos_after_offset; line = 1; column = pos_after_offset + 1 } in
          let range = { match_start = before; match_end = after } in
          match implicit_equals_satisfied !current_environment_ref identifier range content with
          | None -> implicit_equals_match_satisfied := false; return (Unit, acc) (* don't record, unsat *)
          | Some environment ->
            let environment = Environment.add ~range environment identifier content in
            current_environment_ref := environment;
            return (Unit, acc)
        end
      | _ -> return (Unit, acc)

    (* previous r cannot affect control flow match_context to ignore adding a match if a equivalence was refuted *)
    let record_match_context pos_before pos_after rule =
      let open Match.Location in
      if debug then Format.printf "match context start pos: %d@." pos_before;
      if debug then Format.printf "match context end pos %d@." pos_after;
      let extract_matched_text source { offset = match_start; _ } { offset = match_end; _ } =
        if debug then Format.printf "Attempt slice start %d end %d on %S@." match_start match_end source;
        if match_start = 0 && match_end = 0 then
          (* Special case: slice will return the whole string if match_start is
             0 and match_end is 0. It needs to be empty string *)
          ""
        else
          String.slice source match_start match_end
      in
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
      match rule with
      | None ->
        if rewrite then
          begin
            let result, _ = substitute !rewrite_template !current_environment_ref in
            (* Don't just append, but replace the match context including constant
               strings. I.e., somewhere where we are appending the parth that matched, it
               shouldn't, and instead just ignore. *)
            Buffer.add_string actual result;
          end;
        matches_ref := match_context :: !matches_ref
      | Some rule ->
        push_environment_ref := !current_environment_ref;
        push_implicit_equals_match_satisfied := !implicit_equals_match_satisfied;
        (* FIXME Metasyntax should be propagated here. FIXME fresh should be propagated here.*)
        let sat, env = Program.apply ~metasyntax:Metasyntax.default_metasyntax ~substitute_in_place:true rule !current_environment_ref in
        current_environment_ref := !push_environment_ref;
        implicit_equals_match_satisfied := !push_implicit_equals_match_satisfied;
        let new_env = if sat then env else None in
        match new_env with
        | None ->
          if debug then Format.printf "No new_env@.";
          if rewrite then Buffer.add_string actual match_context.matched
        | Some env ->
          if debug then Format.printf "Some new env@.";
          current_environment_ref := env;
          begin
            let result, _ = substitute !rewrite_template !current_environment_ref in
            (* Don't just append, but replace the match context including constant
               strings. I.e., somewhere where we are appending the parth that matched, it
               shouldn't, and instead just ignore. *)
            Buffer.add_string actual result;
          end;
          matches_ref := match_context :: !matches_ref

    let multiline left right =
      let open Parsers.Comments.Omega.Multiline in
      let module M = Make(struct let left = left let right = right end) in
      M.comment

    let until_newline start =
      let open Parsers.Comments.Omega.Until_newline in
      let module M = Make(struct let start = start end) in
      M.comment

    let comment_parser =
      match Language.Syntax.comments with
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
      match Language.Syntax.escapable_string_literals with
      | None -> []
      | Some { delimiters; escape_character } ->
        List.map delimiters ~f:(fun delimiter ->
            escapable delimiter escape_character >>= fun contents ->
            return (f ~contents ~left_delimiter:delimiter ~right_delimiter:delimiter))

    let raw_string_literal_parser (f : 'a literal_parser_callback) =
      choice @@
      List.map Language.Syntax.raw_string_literals ~f:(fun (left_delimiter, right_delimiter) ->
          raw left_delimiter right_delimiter >>= fun contents ->
          return (f ~contents ~left_delimiter ~right_delimiter))

    let until_of_from from =
      Language.Syntax.user_defined_delimiters
      |> List.find_map ~f:(fun (from', until) -> if String.equal from from' then Some until else None)
      |> function
      | Some until -> until
      | None -> assert false

    module Deprecate = struct
      let reserved_delimiters =
        List.concat_map Language.Syntax.user_defined_delimiters ~f:(fun (from, until) -> [from; until])
        |> List.append [":["; "]"]
        |> List.append [":[["; "]]"]

      let reserved =
        reserved_delimiters @ [" "; "\n"; "\t"; "\r"]
        |> List.sort ~compare:(fun v2 v1 ->
            String.length v1 - String.length v2)
    end

    let reserved_holes =
      List.map Template.Matching.hole_parsers ~f:(fun (_, parser) -> parser *> return "")

    let reserved_parsers =
      let user_defined_delimiters =
        List.concat_map Language.Syntax.user_defined_delimiters ~f:(fun (from, until) ->
            [string from; string until]) in
      let user_defined_escapable_strings =
        match Language.Syntax.escapable_string_literals with
        | Some { delimiters; _ } ->
          List.concat_map delimiters ~f:(fun delimiter -> [string delimiter])
        | None -> []
      in
      let user_defined_raw_strings =
        List.concat_map Language.Syntax.raw_string_literals ~f:(fun (from, until) ->
            [string from; string until])
      in
      let user_defined_reserved_comments =
        List.concat_map Language.Syntax.comments ~f:(function
            | Multiline (left, right) -> [string left; string right]
            | Nested_multiline (left, right) -> [string left; string right]
            | Until_newline start -> [string start])
      in
      let spaces = [ " "; "\n"; "\t"; "\r" ] |> List.map ~f:string in
      [ user_defined_delimiters
      ; reserved_holes
      ; user_defined_escapable_strings
      ; user_defined_raw_strings
      ; user_defined_reserved_comments
      ; spaces
      ]
      |> List.concat
      |> choice

    let generate_single_hole_parser () =
      (alphanum <|> char '_') >>| String.of_char

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
          | _ -> Language.Syntax.user_defined_delimiters
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
          | _ -> Language.Syntax.user_defined_delimiters
        in
        List.concat_map parsers ~f:(fun (from, until) -> [from; until])
      in
      fix (fun grammar ->
          let delimsx = between_nested_delims (many grammar) in
          let other = Omega_parser_helper.Deprecate.any_char_except ~reserved >>| String.of_char in
          choice
            [ comment_parser
            ; raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
            ; escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
            ; spaces1
            ; delimsx
            ; other
            ])

    let generate_delimited_hole_parser
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
          | _ -> Language.Syntax.user_defined_delimiters
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
          | _ -> Language.Syntax.user_defined_delimiters
        in
        List.concat_map parsers ~f:(fun (from, until) -> [from; until])
      in
      let inner =
        fix (fun grammar ->
            let delims_over_holes = between_nested_delims (many grammar) in
            let other = Omega_parser_helper.Deprecate.any_char_except ~reserved >>| String.of_char in
            choice
              [ comment_parser
              ; raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
              ; escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents)
              ; spaces1
              ; delims_over_holes
              ; other
              ])
      in
      between_nested_delims (many inner)

    (* this thing is wrapped by a many. also rename it to 'string hole match syntax per char' *)
    let escapable_literal_grammar ~right_delimiter =
      match Language.Syntax.escapable_string_literals with
      | None -> zero
      | Some { escape_character; _ } ->
        choice
          [ (string (Format.sprintf "%c%s" escape_character right_delimiter))
          ; (string (Format.sprintf "%c%c" escape_character escape_character))
          ; (Omega_parser_helper.Deprecate.any_char_except ~reserved:[right_delimiter] >>| String.of_char)
          ]

    let raw_literal_grammar ~right_delimiter =
      (Omega_parser_helper.Deprecate.any_char_except ~reserved:[right_delimiter] >>| String.of_char)

    let sequence_chain ?left_delimiter ?right_delimiter (p_list : (production * 'a) t list) =
      if debug then Format.printf "Sequence chain p_list size: %d@." @@ List.length p_list;
      let i = ref 0 in
      List.fold_right p_list ~init:(return (Unit, acc)) ~f:(fun p acc ->
          let result =
            if debug then Format.printf "iterate fold_right %d@." !i;
            match parse_string ~consume:All p "_signal_hole" with
            | Error s ->
              if debug then Format.printf "Composing p with terminating parser, error %s@." s;
              p *> acc
            | Ok (Hole { sort; identifier; dimension; _ }, user_state) ->
              begin
                match sort with
                | Regex ->
                  let separator = List.find_map_exn Meta.syntax ~f:(function
                      | Hole _ -> None
                      | Regex (_, separator, _) -> Some separator)
                  in
                  let identifier, pattern = String.lsplit2_exn identifier ~on:separator in (* FIXME parse *)
                  let identifier = if String.(identifier = "") then wildcard else identifier in
                  if debug then Format.printf "Regex: Id: %s Pat: %s@." identifier pattern;
                  let pattern, prefix =
                    if String.is_prefix pattern ~prefix:"^" then
                      (* FIXME: match beginning of input too *)
                      String.drop_prefix pattern 1,
                      Some (
                        (char '\n' *> return "")
                        <|>
                        (pos >>= fun p -> if p = 0 then return "" else fail "")
                      )
                    else
                      pattern, None
                  in
                  let pattern, suffix =
                    if String.is_suffix pattern ~suffix:"$" then
                      String.drop_suffix pattern 1, Some (char '\n' *> return "" <|> end_of_input *> return "")
                    else
                      pattern, None
                  in
                  let compiled_regexp = Regexp.PCRE.make_regexp pattern in
                  let regexp_parser = Regexp.PCRE.regexp compiled_regexp in
                  let regexp_parser =
                    match prefix, suffix with
                    | Some prefix, None -> prefix *> regexp_parser
                    | None, Some suffix -> regexp_parser <* suffix
                    | Some prefix, Some suffix -> prefix *> regexp_parser <* suffix
                    | None, None -> regexp_parser
                  in
                  (* the eof matters here for that one tricky test case *)
                  let base_parser =
                    [ regexp_parser
                    ; end_of_input >>= fun () -> return ""
                    ]
                  in
                  pos >>= fun offset ->
                  choice base_parser >>= fun value ->
                  if debug then Format.printf "Regex match @@ %d value %s@." offset value;
                  acc >>= fun _ ->
                  let m =
                    { offset
                    ; identifier
                    ; text = value
                    }
                  in
                  r user_state (Match m)
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
                       Omega_parser_helper.ignore acc)
                  in
                  (
                    pos >>= fun pos ->
                    if get_pos () = (-1) then set_pos pos;
                    let stop_at = choice [ rest; Omega_parser_helper.ignore reserved_parsers ] in
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
                    many (Omega_parser_helper.Deprecate.any_char_except ~reserved:["\n"])
                    >>| fun x -> [(String.of_char_list x)^"\n"]
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
                | Expression ->
                  let first_pos = ref (-1) in
                  let set_pos v = first_pos := v in
                  let get_pos () = !first_pos in
                  let _non_space : string t =
                    let rest =
                      if !i = 0 then end_of_input
                      else Omega_parser_helper.ignore acc
                    in
                    (
                      pos >>= fun pos ->
                      if get_pos () = (-1) then set_pos pos;
                      let stop_at = choice [ rest; Omega_parser_helper.ignore reserved_parsers ] in
                      many1_till_stop any_char stop_at (* Beware of this use. *)
                    ) >>| String.of_char_list
                  in
                  let non_space =
                    many1 (Omega_parser_helper.Deprecate.any_char_except ~reserved:([" "]@Deprecate.reserved_delimiters)) >>| String.of_char_list
                  in
                  let delimited =
                    (* IDK why this rest works without end_of_input but it's needed for non_space. *)
                    let rest = Omega_parser_helper.ignore acc in
                    (many1_till
                       (pos >>= fun pos ->
                        if debug then Format.printf "Pos is %d@." pos;
                        if get_pos () = (-1) then set_pos pos;
                        (match dimension with
                         | Code ->
                           generate_delimited_hole_parser
                             ?priority_left_delimiter:left_delimiter
                             ?priority_right_delimiter:right_delimiter
                             ()
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
                  (many1 @@ choice [non_space; delimited])
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
                  (* change this so that rest is not consumed *)
                  let rest =
                    (* if this is the base case (the first time we go around the
                       loop backwards, when the first parser is a hole), then it
                       means there's a hole at the end without anything following
                       it in the template. So it should always match to
                       end_of_input (not empty string) *)
                    if !i = 0 then
                      (if debug then Format.printf "hole everything until: match to the end of this level@.";
                       end_of_input)
                    else
                      (if debug then Format.printf "hole everything until: append suffix@.";
                       Omega_parser_helper.ignore acc)
                  in
                  let first_pos = ref (-1) in
                  let set_pos v = first_pos := v in
                  let get_pos () = !first_pos in
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
                  if debug then Format.printf "Recording!@.";
                  r user_state (Match m)
              end
            | Ok (_, _user_state) -> failwith "unreachable: _signal_hole parsed but not handled by Hole variant"
          in
          i := !i + 1;
          result)

    let generate_pure_spaces_parser _ignored =
      spaces1 >>= fun s1 -> r acc (Template_string s1)

    (* XXX change ignore to unit once everything works.
       right now it's the string that was parsed by spaces1 *)
    let generate_spaces_parser _ignored =
      (* XXX still some parts ignored in the choice case in Alpha *)
      if debug then Format.printf "Template_spaces(%s)@." _ignored;
      many1 @@
      choice
        [ comment_parser
        ; spaces1
        ] >>= fun result ->
      r acc (Template_string (String.concat result))

    (** All code can have comments interpolated *)
    let generate_string_token_parser str =
      if debug then Format.printf "Template_string(%s)@." str;
      many comment_parser
      >>= fun s1 ->
      string str >>= fun result ->
      r acc (Template_string (String.concat s1 ^ result))

    let hole_parser sort dimension : (production * 'a) t t =
      let hole_parser = (* FIXME try make it List.find *)
        let open Polymorphic_compare in
        List.fold ~init:[] Template.Matching.hole_parsers ~f:(fun acc (sort', parser) ->
            if sort' = sort then parser::acc else acc)
      in
      let skip_signal hole = Omega_parser_helper.ignore (string "_signal_hole") >>| fun () -> (Hole hole, acc) in
      match hole_parser with
      | [] -> fail "none" (* not defined *)
      | l ->
        choice l >>| function identifier ->
          skip_signal { sort; identifier; dimension; at_depth = None }

    let generate_hole_for_literal sort ~contents ~left_delimiter ~right_delimiter () =
      let literal_holes =
        Types.Hole.sorts ()
        |> List.map ~f:(fun kind -> hole_parser kind sort) (* Note: Uses attempt in alpha *)
        |> choice
      in
      let reserved_holes = List.map reserved_holes ~f:Omega_parser_helper.skip in
      let other = Omega_parser_helper.(
          up_to @@
          choice
            [ (spaces1 *> return ())
            ; (choice reserved_holes *> return ())
            ]
          >>| String.of_char_list)
      in
      let parser =
        many @@
        choice
          [ literal_holes
          ; (spaces1 >>| generate_pure_spaces_parser)
          ; (other >>| generate_string_token_parser)
          ]
      in
      match parse_string ~consume:All parser contents with
      | Ok parsers -> sequence_chain ~left_delimiter ~right_delimiter parsers
      | Error _ ->
        failwith "If this failure happens it is a bug: Converting a \
                  quoted string in the template to a parser list should \
                  not fail here"

    let general_parser_generator rule : (production * 'a) t t =
      let spaces : (production * 'a) t t =
        lift
          (fun result -> generate_spaces_parser (String.concat result))
          (many1 (comment_parser <|> spaces1))
      in
      let other =
        (many1 (Omega_parser_helper.Deprecate.any_char_except ~reserved:Deprecate.reserved) >>| String.of_char_list)
        >>| generate_string_token_parser
      in
      let code_holes =
        Types.Hole.sorts ()
        |> List.map ~f:(fun kind -> hole_parser kind Code)
        |> choice
      in
      fix (fun (generator : (production * 'a) t list t) ->
          if debug then Format.printf "Descends@.";
          let nested =
            if debug then Format.printf "Nested@.";
            choice @@
            List.map Language.Syntax.user_defined_delimiters ~f:(fun (left_delimiter, right_delimiter) ->
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
      >>| fun p_list ->
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
          if !implicit_equals_match_satisfied then record_match_context start_pos end_pos rule;
          implicit_equals_match_satisfied := true; (* reset *)
          current_environment_ref := Match.Environment.create ();
          r acc Unit
        | Fuzzy ->
          let prefix =
            choice
              [ comment_parser
              ; (raw_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents))
              ; (escapable_string_literal_parser (fun ~contents ~left_delimiter:_ ~right_delimiter:_ -> contents))
              ; any_char >>| Char.to_string
              ]
          in
          let match_one =
            pos >>= fun start_pos ->
            current_environment_ref := Match.Environment.create ();
            consumed matcher >>= fun value ->
            if debug then Format.printf "Full match context result: %s@." value;
            pos >>= fun end_pos ->
            let start_pos =
              if String.length value = 0 then
                start_pos (*offset + 1 this may not matter, if we correct for the whole match conext *)
              else
                start_pos
            in
            (if String.length value = 0 then
               advance 1
             else
               return ()) >>= fun () ->
            if debug then Format.printf "Calculated end_pos %d@." end_pos;
            if !implicit_equals_match_satisfied then record_match_context start_pos end_pos rule;
            implicit_equals_match_satisfied := true; (* reset *)
            current_environment_ref := Match.Environment.create ();
            return (Unit, "")
          in
          (* many1 may be appropriate *)
          let prefix = (prefix >>= fun s -> r acc (String s)) in
          let first_match_attempt = choice [match_one; prefix] in (* consumes a character in prefix if no match *)
          let matches = many_till first_match_attempt end_of_input in
          matches >>= fun _result ->
          r acc Unit

    let to_template template rule =
      let state = Buffered.parse (general_parser_generator rule) in
      let state = Buffered.feed state (`String template) in
      Buffered.feed state `Eof
      |> function
      | Buffered.Done ({ len; _ }, p) ->
        if len <> 0 then failwith @@ Format.sprintf "Input left over in template where not expected: %d" len;
        Ok p
      | _ -> Or_error.error_string "Template could not be parsed."

    let run_the_parser_for_first p source : Match.t Or_error.t =
      push_source_ref := !source_ref;
      source_ref := source;
      let state = Buffered.parse p in
      let state = Buffered.feed state (`String source) in
      let state = Buffered.feed state `Eof in
      match state with
      | Buffered.Done ({ len; off; _ }, (_, _result_string)) ->
        source_ref := !push_source_ref;
        if rewrite then Format.eprintf "Result string:@.---@.%s---@." @@ Buffer.contents actual;
        if len <> 0 then
          (if debug then Format.eprintf "Input left over in parse where not expected: off(%d) len(%d)" off len;
           Or_error.error_string "Does not match template")
        else
          Ok (Match.create ()) (* Fake for now *)
      | _ ->
        source_ref := !push_source_ref;
        Or_error.error_string "No matches"

    let first_is_broken ?configuration:_ ?shift:_ template source rule : Match.t Or_error.t =
      match to_template template rule with
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

    let all ?configuration ?(rule = [Types.Ast.True]) ~template ~source:original_source () : Match.t list =
      push_matches_ref := !matches_ref;
      configuration_ref := Option.value configuration ~default:!configuration_ref;
      let Rule.{ nested } = Rule.options rule in
      let rec aux_all ?configuration ?(nested = false) ~template ~source () =
        matches_ref := [];
        if String.is_empty template && String.is_empty source then [trivial]
        else match first_is_broken template source (Some rule) with (* FIXME always Some rule *)
          | Ok _
          | Error _ ->
            let matches = List.rev !matches_ref in
            if nested then
              (compute_nested_matches ?configuration ~nested template matches) @ matches
            else
              matches
      and compute_nested_matches ?configuration ?nested template matches =
        let open Match in
        let open Range in
        let rec aux acc matches =
          match (matches : Match.t list) with
          | [] -> acc
          | { environment; _ }::rest ->
            List.fold ~init:acc (Environment.vars environment) ~f:(fun acc v ->
                let source_opt = Environment.lookup environment v in
                match source_opt with
                | Some source ->
                  let nested_matches =
                    let matches = aux_all ?configuration ?nested ~template ~source () in
                    let { match_start = ms; _ } = Option.value_exn (Environment.lookup_range environment v) in
                    List.map matches ~f:(fun m ->
                        let environment =
                          List.fold (Environment.vars m.environment) ~init:m.environment ~f:(fun env var ->
                              let open Option in
                              let updated : environment option =
                                Environment.lookup_range env var
                                >>| fun r ->
                                let range = {
                                  match_start =
                                    { r.match_start with offset = ms.offset + r.match_start.offset } ;
                                  match_end =
                                    { r.match_end with offset = ms.offset + r.match_end.offset }
                                }
                                in
                                Environment.update_range env var range
                              in
                              match updated with
                              | None -> env
                              | Some env -> env)
                        in
                        let range = {
                          match_start =
                            { m.range.match_start with offset = ms.offset + m.range.match_start.offset  } ;
                          match_end =
                            { m.range.match_end with offset = ms.offset + m.range.match_end.offset }
                        }
                        in
                        { m with range; environment })
                  in
                  acc @ nested_matches
                | _ -> acc)
            @ aux acc rest
        in
        aux [] matches
      in
      let result =
        if nested then
          let open Match in
          (* Use sort on offset for a top-down ordering. *)
          aux_all ?configuration ~nested ~template ~source:original_source ()
          |> List.sort ~compare:(fun left right -> left.range.match_start.offset - right.range.match_start.offset)
        else
          (* Don't reverse the list for non-nested matches--it matters for rewriting. *)
          aux_all ?configuration ~nested ~template ~source:original_source ()
      in
      matches_ref := !push_matches_ref;
      result

    let first ?configuration ?shift:_ template source : Match.t Or_error.t =
      configuration_ref := Option.value configuration ~default:!configuration_ref;
      matches_ref := [];
      match all ?configuration ~template ~source () with
      | [] -> Or_error.error_string "No result"
      | (hd::_) -> Ok hd (* FIXME be efficient *)
  end

  and Program : sig
    val apply
      :  ?substitute_in_place:bool
      -> ?metasyntax:Metasyntax.t
      -> Rule.t
      -> Match.environment
      -> Evaluate.result
  end = struct
    let apply
        ?(substitute_in_place = true)
        ?metasyntax
        rule
        env =
      Evaluate.apply
        ~substitute_in_place
        ?metasyntax
        ~match_all:(Matcher.all ~rule:[Types.Ast.True])
        rule
        env
  end

  include Matcher
end
