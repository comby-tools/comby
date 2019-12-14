open Core
open Angstrom

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


let record_match_context pos_before pos_after =
  let open Match.Location in
  if debug then Format.printf "match context start pos: %d@." pos_before;
  if debug then Format.printf "match context end pos %d@." pos_after;
  (* FIXME this may be slow. Try (a) collecting this
     or (b) removing it by just doing a rewrite *)
  let extract_matched_text source { offset = match_start; _ } { offset = match_end; _ } =
    String.slice source match_start match_end
  in
  let match_context =
    let match_start =
      { default with offset = pos_before }
    in
    let match_end =
      { default with offset = pos_after }
    in
    Match.
      { range = { match_start; match_end }
      ; environment = !current_environment_ref
      ; matched = extract_matched_text !source_ref match_start match_end
      }
  in
  matches_ref := match_context :: !matches_ref

module Make (Syntax : Syntax.S) (Info : Info.S) = struct
  include Info

  (* This is the init we will pass in with a functor later *)
  let acc = 0

  (* This is the function we will pass in with a functor later *)
  let f acc _production =
    acc + 1

  let r acc production : (production * 'a) t =
    let open Match in
    let open Range in
    let acc = f acc production in
    match production with
    | String s ->
      if debug then Format.printf "Matched String: %S@." s;
      return (Unit, acc)
    | Match { offset = pos_after; identifier; text = content } ->
      (* using just pos after for now, because thats what we do in matcher. lol *)
      if debug then Format.printf "Match: %S @@ %d for %s@." content pos_after identifier;
      let before = Location.default in (* FIXME *)
      let after = { Location.default with offset = pos_after } in
      let range = { match_start = before; match_end = after } in
      let environment = Environment.add ~range !current_environment_ref identifier content in
      current_environment_ref := environment;
      return (Unit, acc)
    | _ -> return (Unit, acc)

  let between left right p =
    left *> p <* right

  let zero =
    fail ""

  let comment_parser =
    match Syntax.comments with
    | [] -> zero
    | syntax ->
      List.map syntax ~f:(function
          | Multiline (left, right) ->
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

  let escapable_string_literal_parser =
    (match Syntax.escapable_string_literals with
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

  let until_of_from from =
    Syntax.user_defined_delimiters
    |> List.find_map ~f:(fun (from', until) -> if from = from' then Some until else None)
    |> function
    | Some until -> until
    | None -> assert false

  let alphanum =
    satisfy (function
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9' -> true
        | _ -> false)

  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

  let reserved_delimiters =
    List.concat_map Syntax.user_defined_delimiters ~f:(fun (from, until) -> [from; until])
    |> List.append [":["; "]"]
    |> List.append [":[["; "]]"]

  let reserved =
    reserved_delimiters @ [" "; "\n"; "\t"; "\r"]
    |> List.sort ~compare:(fun v2 v1 ->
        String.length v1 - String.length v2)

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

  let generate_single_hole_parser () =
    (alphanum <|> char '_') |>> String.of_char

  let generate_greedy_hole_parser
      ?priority_left_delimiter:left_delimiter
      ?priority_right_delimiter:right_delimiter
      () =
    let between_nested_delims p from =
      let until = until_of_from from in
      between (string from) (string until) p
      >>= fun result -> return (String.concat @@ [from] @ result @ [until])
    in
    let between_nested_delims p =
      (match left_delimiter, right_delimiter with
       | Some left_delimiter, Some right_delimiter -> [ (left_delimiter, right_delimiter) ]
       | _ -> Syntax.user_defined_delimiters)
      |> List.map ~f:fst
      |> List.map ~f:(between_nested_delims p)
      |> choice
    in
    let reserved =
      (match left_delimiter, right_delimiter with
       | Some left_delimiter, Some right_delimiter -> [ (left_delimiter, right_delimiter) ]
       | _ -> Syntax.user_defined_delimiters)
      |> List.concat_map ~f:(fun (from, until) -> [from; until])
    in
    fix (fun grammar ->
        let delimsx = between_nested_delims (many grammar) in
        let other = any_char_except ~reserved |>> String.of_char in
        (* FIXME holes does not handle space here, but does in alpha *)
        choice
          [ comment_parser
          ; escapable_string_literal_parser
          ; delimsx
          ; other
          ])

  let cons x xs = x :: xs

  let many_till p t =
    fix (fun m -> (t *> return []) <|> (lift2 cons p m))

  let many1_till p t =
    lift2 cons p (many_till p t)

  let sequence_chain (p_list : (production * 'a) t list) =
    let i = ref 0 in
    List.fold_right p_list ~init:(return (Unit, acc)) ~f:(fun p acc ->
        let result =
          if debug then Format.printf "iterate fold_right %d@." !i;
          match parse_string p "_signal_hole" with
          | Error _ ->
            if debug then Format.printf "Composing p with terminating parser@.";
            p *> acc
          | Ok (Hole { sort; identifier; _ }, user_state) ->
            (*Format.printf "Ok.@.";*)
            begin
              match sort with
              | Alphanum ->
                pos >>= fun pos_before ->
                many1 (generate_single_hole_parser ())
                >>= fun value ->
                (* acc must come after in order to sat. try mimic alpha to better express this. *)
                acc >>= fun _ ->
                r user_state
                  (Match
                     { offset = pos_before; identifier; text = (String.concat value) }
                  )
              | Everything ->
                if debug then Format.printf "do hole %s@." identifier;
                let first_pos = Set_once.create () in
                let pparser =
                  let until =
                    (* if this is the base case (the first time we go around the
                       loop backwards, when the first parser is a hole), then it
                       means there's a hole at the end without anything following
                       it in the template. So it should always match to
                       end_of_input (not empty string) *)
                    if !i = 0 then
                      (if debug then Format.printf "Yes this case@.";
                       end_of_input)
                    else
                      (if debug then Format.printf "Yes this second case@.";
                       acc >>= fun _ -> return ())
                  in
                  (many_till
                     (pos >>= fun pos -> Set_once.set_if_none first_pos [%here] pos;
                      generate_greedy_hole_parser ())
                     (pos >>= fun pos -> Set_once.set_if_none first_pos [%here] pos;
                      until)
                     (* it may be that the many till for the first parser
                        succeeds on 'empty string', specifically in the :[1]:[2]
                        case for :[1]. We won't capture the pos of :[1] in the
                        first parser since it doesn't fire, so, so we have to
                        set the pos right before the until parser below, if that
                        happens. *)
                  ) >>| String.concat
                in
                pparser >>= fun text ->
                (*Format.printf "have results %d@." @@ List.length results;*)
                let offset =
                  match Set_once.get first_pos with
                  | Some offset -> offset
                  | _ -> failwith "Did not expect unset offset"
                in
                r
                  user_state
                  (Match
                     { offset
                     ; identifier
                     ; text
                     })
              | _ -> assert false (* TODO: other sorts *)
            end
          | Ok (_, _user_state) -> failwith "unreachable: _signal_hole parsed but not handled by Hole variant"
        in
        i := !i + 1;
        result)

  (** must have at least one, otherwise spins on
      the empty string *)
  let spaces1 =
    satisfy is_whitespace >>= fun c ->
    (* XXX use skip_while once everything works.
       we don't need the string *)
    take_while is_whitespace >>= fun s ->
    return (Format.sprintf "%c%s" c s)

  let spaces =
    take_while is_whitespace >>= fun s ->
    return s

  (* XXX change ignore to unit once everything works.
     right now it's the string that was parsed by spaces1 *)
  let generate_spaces_parser _ignored =
    (* XXX still some parts ignored in the choice case in Alpha *)
    if debug then Format.printf "Template_spaces(%s)@." _ignored;
    spaces1 *> many comment_parser <* spaces
    >>= fun result -> r acc (String (String.concat result))

  (** All code can have comments interpolated *)
  let generate_string_token_parser str =
    if debug then Format.printf "Template_string(%s)@." str;
    many comment_parser
    *> string str
    *> many comment_parser
    >>= fun result -> r acc (String (String.concat result))

  let skip_unit p =
    p |>> ignore

  let identifier_parser () =
    many (alphanum <|> char '_')
    |>> String.of_char_list

  let single_hole_parser () =
    string ":[[" *> identifier_parser () <* string "]]"

  let greedy_hole_parser () =
    string ":[" *> identifier_parser () <* string "]"

  let many1_till p t =
    let cons x xs = x::xs in
    lift2 cons p (many_till p t)

  let hole_parser sort dimension : (production * 'a) t t =
    let open Hole in
    let hole_parser =
      match sort with
      | Alphanum -> single_hole_parser ()
      | Everything -> greedy_hole_parser ()
      | _ -> failwith "not implemented"
    in
    let skip_signal hole = skip_unit (string "_signal_hole") |>> fun () -> (Hole hole, acc) in
    hole_parser |>> fun identifier -> skip_signal { sort; identifier; dimension; optional = false }

  let general_parser_generator : (production * 'a) t t =
    fix (fun (generator : (production * 'a) t list t) ->
        if debug then Format.printf "Descends@.";
        let nested =
          (* FIXME nested needs comments and string literals (or does it not
             need one for string literals because we handle it in fix? Unsure,
             couldn't come up with a CLI test. Check against test suite. *)
          if debug then Format.printf "Nested@.";
          Syntax.user_defined_delimiters
          |> List.map ~f:(fun (left_delimiter, right_delimiter) ->
              (string left_delimiter
               *> generator
               <* string right_delimiter)
              >>= fun (g: (production * 'a) t list) ->
              if debug then Format.printf "G size: %d; delim %s@." (List.length g) left_delimiter;
              (([string left_delimiter
                 >>= fun result -> r acc (String result)]
                @ g
                @ [ string right_delimiter
                    >>= fun result -> r acc (String result)])
               |>
               sequence_chain)
              |> return)
          |> choice
        in
        let spaces : (production * 'a) t t = spaces1 |>> generate_spaces_parser in
        let escapable_string_literal_parser : (production * 'a) t t =
          escapable_string_literal_parser
          >>| fun string_literal_contents ->
          (* FIXME incomplete likely, may need info about delims. also, no hole
             matching yet. *)
          generate_string_token_parser string_literal_contents
        in
        let other =
          (* XXX many1_till would be cool, but it also parses the thing that
             causes it to fail, which i need restored.  many_till is 'parse and
             include the parse of the exception', whereas I want parse and
             exclude the parse of the exception (hard to reintroduce ) *)
   (*
   (many1_till (any_char >>= fun c -> Format.printf "parsed %c@." c;
   return c) (List.map reserved ~f:string |> choice >>= fun x ->
   Format.printf "Fail on %s@." x; return x) |>> fun s ->
   Format.printf "Chars: %s@." @@ String.of_char_list s;
   String.of_char_list s) *)
          (many1 (any_char_except ~reserved) |>> String.of_char_list)
          |>> fun x ->
          if debug then Format.printf "Other: %s@." x;
          generate_string_token_parser x
        in
        if debug then Format.printf "Many... @.";
        (* can't be many1 because then {} isn't a valid template (delimiters have to
           contain something then and can't be empty *)
        (* don't want it to be many because empty string will satisfy and
           "" is a valid template, or even "{", because it generates 'seq' on chain *)
        many @@
        choice
          [ hole_parser Alphanum Code
          ; hole_parser Everything Code
          ; escapable_string_literal_parser
          ; spaces
          ; nested
          ; other
          ]
        >>= fun x ->
        if debug then Format.printf "Produced %d parsers in main generator@." @@ List.length x;
        return x
      )
    |>> fun p_list ->
    p_list
    |> sequence_chain
    |> fun matcher ->
    (* FIXME: skip_unit needs to be raw literals *)
    (* XXX: what is the difference does many vs many1 make here? Semantically,
       it should mean "0 or more matching contexts" vs "1 or more matching
       contexts". We only care about the 1 case anyway, so... *)
    many (skip_unit
            (many_till (skip_unit comment_parser
                        <|> skip_unit escapable_string_literal_parser
                        <|> skip_unit any_char)
               (
                 at_end_of_input >>= fun res ->
                 if debug then Format.printf "We are at the end? %b.@." res;
                 if res then
                   (if debug then Format.printf "We ended@.";
                    fail "x")
                 else
                   (* we found a match *)
                   pos >>= fun start_pos ->
                   matcher >>= fun _access_last_production_herpe ->
                   pos >>= fun end_pos ->
                   record_match_context start_pos end_pos;
                   current_environment_ref := Match.Environment.create ();
                   return Unit)
               (*<|>
                 (end_of_input >>= fun () -> return Unit)*)
            )
         )
    (*>>= fun _x -> end_of_input *)
    >>= fun _x -> r acc Unit

  let to_template template =
    let state = Buffered.parse general_parser_generator in
    let state = Buffered.feed state (`String template) in
    Buffered.feed state `Eof
    |> function
    | Buffered.Done ({ len; _ }, p) ->
      if len <> 0 then failwith @@
        Format.sprintf "Input left over in template where not expected: %d" len;
      Ok p
    | _ -> Or_error.error_string "Template could not be parsed."

  let run_the_parser_for_first p source : Match.t Or_error.t =
    source_ref := source;
    let state = Buffered.parse p in
    let state = Buffered.feed state (`String source) in
    let state = Buffered.feed state `Eof in
    match state with
    | Buffered.Done ({ len; off; _ }, _result) ->
      if len <> 0 then
        (if debug then
           Format.eprintf "Input left over in parse where not expected: off(%d) len(%d)" off len;
         Or_error.error_string "Does not match tempalte")
      else
        Ok (Match.create ()) (* Fake for now *)
    | _ -> Or_error.error_string "No matches"

  let first_broken ?configuration:_ ?shift:_ template source : Match.t Or_error.t =
    match to_template template with
    | Ok p ->
      begin match run_the_parser_for_first p source with
        | Ok _ -> (* matches passed, ok to access *)
          begin
            match !matches_ref with
            | [] -> Or_error.error_string "not really"
            | hd::_ -> Ok hd
          end
        | Error e -> (* parse failed *)
          Error e
      end
    | Error e ->
      Format.printf "Template FAIL %s@." @@ Error.to_string_hum e;
      Error e

  let all ?configuration:_ ~template ~source : Match.t list =
    matches_ref := [];
    match first_broken template source with
    | Ok _
    | Error _ -> List.rev !matches_ref

  let first ?configuration ?shift:_ template source : Match.t Or_error.t =
    matches_ref := [];
    match all ?configuration ~template ~source with
    | [] -> Or_error.error_string "nothing"
    | (hd::_) as m ->
      if debug then List.iter m ~f:(fun { environment; _ } ->
          Format.printf "START:@.%s@.END@." (Match.Environment.to_string environment));
      Ok hd
end
