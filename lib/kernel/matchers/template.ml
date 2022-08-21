open Vangstrom
open Core_kernel
open Match
open Types.Template

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

module Make (Metasyntax : Types.Metasyntax.S) (External : Types.External.S) : Types.Template.S =
struct
  let up_to p = many1 (not_followed_by p *> any_char)
  let optional d = Option.value d ~default:""
  let character () = choice @@ List.map ~f:char (String.to_list Metasyntax.identifier)
  let identifier () = many1 @@ character () >>| String.of_char_list

  let regex_expression suffix =
    lift
      String.concat
      (many1
      @@ fix (fun expr ->
           choice
             [ lift
                 (fun x -> Format.sprintf "[%s]" @@ String.concat x)
                 (char '[' *> many1 expr <* char ']')
             ; lift (fun c -> Format.sprintf {|\%c|} c) (char '\\' *> any_char)
             ; lift String.of_char_list (up_to (string suffix))
             ]))

  let regex_body separator suffix =
    both (option "" (identifier ())) (char separator *> regex_expression suffix)

  (** Parsers for Matching. Different from rewrite templates which can have :[x].attribute *)
  module Matching = struct
    (** Folds left to respect order of definitions in custom metasyntax for
        matching, where we attempt to parse in order. Note this is significant if a
        syntax like $X~regex should be tried before shortcircuiting on $X, in which
        case it should be defined _after_ the $X syntax (most general should be
        first). *)
    let hole_parsers =
      (* hole parsers for match templates only *)
      List.fold ~init:[] Metasyntax.syntax ~f:(fun acc v ->
        let result =
          match v with
          | Hole (sort, Delimited (left, right)) ->
            ( sort
            , lift3
                (fun _left v _right -> v)
                (string (optional left))
                (identifier ())
                (string (optional right)) )
          | Hole (sort, Reserved_identifiers l) -> sort, choice (List.map l ~f:string)
          | Regex (left, separator, right) ->
            ( Regex
            , (* matcher wants <identifier><sep><expr> and splits it later. Fix
                         this later to give v and pattern only *)
              lift3
                (fun _left (v, expr) _right -> Format.sprintf "%s%c%s" v separator expr)
                (string left)
                (regex_body separator right)
                (string right) )
        in
        result :: acc)
  end

  let attribute_to_kind = function
    | "value" -> Value
    | "length" -> Length
    | "lines" -> Lines
    | "offset" | "offset.start" -> OffsetStart
    | "offset.end" -> OffsetEnd
    | "line" | "line.start" -> LineStart
    | "line.end" -> LineEnd
    | "column" | "column.start" -> ColumnStart
    | "column.end" -> ColumnEnd
    | "file" | "file.path" -> FilePath
    | "file.name" -> FileName
    | "file.directory" -> FileDirectory
    | "lowercase" -> Lowercase
    | "UPPERCASE" -> Uppercase
    | "Capitalize" -> Capitalize
    | "uncapitalize" -> Uncapitalize
    | "UpperCamelCase" -> UpperCamelCase
    | "lowerCamelCase" -> LowerCamelCase
    | "UPPER_SNAKE_CASE" -> UpperSnakeCase
    | "lower_snake_case" -> LowerSnakeCase
    | "lsif.hover" -> External "lsif.hover"
    | s -> failwith @@ Format.sprintf "invalid attribute %S" s

  let attribute_access () =
    char '.'
    *> choice
         [ string "value"
         ; string "length"
         ; string "lines"
         ; string "offset.start"
         ; string "offset.end"
         ; string "offset"
         ; string "line.start"
         ; string "line.end"
         ; string "line"
         ; string "column.start"
         ; string "column.end"
         ; string "column"
         ; string "file.path"
         ; string "file.name"
         ; string "file.directory"
         ; string "file"
         ; string "lowercase"
         ; string "UPPERCASE"
         ; string "Capitalize"
         ; string "uncapitalize"
         ; string "UpperCamelCase"
         ; string "lowerCamelCase"
         ; string "UPPER_SNAKE_CASE"
         ; string "lower_snake_case"
         ; string "lsif.hover"
         ]
    <* not_followed_by Omega_parser_helper.alphanum

  (** Folds left to respect order of definitions in custom metasyntax for
      matching, where we attempt to parse in order. Note this is significant if a
      syntax like $X~regex should be tried before shortcircuiting on $X, in which
      case it should be defined _after_ the $X syntax (most general should be
      first). *)
  let rewrite_hole_parsers =
    List.fold ~init:[] Metasyntax.syntax ~f:(fun acc v ->
      let result =
        match v with
        | Hole (_, Delimited (left, right)) ->
          lift4
            (fun left v right kind ->
              let dot_attribute = if String.(kind = "value") then "" else "." ^ kind in
              Format.sprintf "%s%s%s%s" left v right dot_attribute, v, kind)
            (string (optional left))
            (identifier ())
            (string (optional right))
            (option "value" (attribute_access ()))
        | Hole (_, Reserved_identifiers l) ->
          lift2
            (fun v kind ->
              let dot_attribute = if String.(kind = "value") then "" else "." ^ kind in
              Format.sprintf "%s%s" v dot_attribute, v, kind)
            (choice (List.map l ~f:string))
            (option "value" (attribute_access ()))
        | Regex (left, separator, right) ->
          lift4
            (fun left (v, expr) right kind ->
              let dot_attribute = if String.(kind = "value") then "" else "." ^ kind in
              Format.sprintf "%s%s%c%s%s%s" left v separator expr right dot_attribute, v, kind)
            (string left)
            (regex_body separator right)
            (string right)
            (option "value" (attribute_access ()))
      in
      result :: acc)

  let parse_template =
    let hole = choice rewrite_hole_parsers in
    many
    @@ choice
         [ (pos
           >>= fun offset ->
           hole
           >>| fun (pattern, variable, kind) ->
           Hole { pattern; variable; offset; kind = attribute_to_kind kind })
         ; (up_to (choice rewrite_hole_parsers) >>| fun c -> Constant (String.of_char_list c))
         ]

  let parse template =
    match parse_string ~consume:All parse_template template with
    | Ok result -> result
    | Error e -> failwith ("No rewrite template parse: " ^ e)

  let variables template =
    parse template
    |> List.filter_map ~f:(function
         | Hole { pattern; variable; offset; kind } -> Some { pattern; variable; offset; kind }
         | _ -> None)

  let to_string template =
    let buf = Buffer.create 10 in
    List.iter template ~f:(function
      | Constant c -> Buffer.add_string buf c
      | Hole { pattern; _ } -> Buffer.add_string buf pattern);
    Buffer.contents buf

  let camel_to_snake s =
    let rec aux i = function
      | [] -> []
      | ('A' .. 'Z' as c) :: tl when i = 0 -> Char.lowercase c :: aux (i + 1) tl
      | ('A' .. 'Z' as c) :: tl when i <> 0 -> '_' :: Char.lowercase c :: aux (i + 1) tl
      | c :: tl -> c :: aux (i + 1) tl
    in
    aux 0 (String.to_list s) |> String.of_char_list

  let substitute_kind ?filepath { variable; kind; _ } env =
    let open Option in
    let length_to_string n = Format.sprintf "%d" (String.length n) in
    match kind with
    | Value -> Environment.lookup env variable
    | Length -> Environment.lookup env variable >>| length_to_string
    | Lines ->
      Environment.lookup env variable
      >>| String.count ~f:(Char.( = ) '\n')
      >>| (fun v -> if v = 0 then 1 else v)
      >>| Int.to_string
    | OffsetStart ->
      Environment.lookup_range env variable
      >>| fun { match_start = { offset; _ }; _ } -> Int.to_string offset
    | OffsetEnd ->
      Environment.lookup_range env variable
      >>| fun { match_end = { offset; _ }; _ } -> Int.to_string offset
    | LineStart ->
      filepath
      >>= fun filepath ->
      Environment.lookup_range env variable
      >>| fun { match_start = { offset; _ }; _ } ->
      let source = In_channel.read_all filepath in
      (* Inefficient. *)
      let index = Match.Offset.index ~source in
      let line, _ = Match.Offset.convert_fast ~offset index in
      Int.to_string line
    | LineEnd ->
      filepath
      >>= fun filepath ->
      Environment.lookup_range env variable
      >>| fun { match_end = { offset; _ }; _ } ->
      let source = In_channel.read_all filepath in
      (* Inefficient. *)
      let index = Match.Offset.index ~source in
      let line, _ = Match.Offset.convert_fast ~offset index in
      Int.to_string line
    | ColumnStart ->
      filepath
      >>= fun filepath ->
      Environment.lookup_range env variable
      >>| fun { match_start = { offset; _ }; _ } ->
      let source = In_channel.read_all filepath in
      (* Inefficient. *)
      let index = Match.Offset.index ~source in
      let _, column = Match.Offset.convert_fast ~offset index in
      Int.to_string column
    | ColumnEnd ->
      filepath
      >>= fun filepath ->
      Environment.lookup_range env variable
      >>| fun { match_end = { offset; _ }; _ } ->
      let source = In_channel.read_all filepath in
      (* Inefficient. *)
      let index = Match.Offset.index ~source in
      let _, column = Match.Offset.convert_fast ~offset index in
      Int.to_string column
    | FilePath -> filepath
    | FileName -> filepath >>| Filename.basename
    | FileDirectory -> filepath >>| Filename.dirname
    | Lowercase -> Environment.lookup env variable >>| String.lowercase
    | Uppercase -> Environment.lookup env variable >>| String.uppercase
    | Capitalize -> Environment.lookup env variable >>| String.capitalize
    | Uncapitalize -> Environment.lookup env variable >>| String.uncapitalize
    | UpperCamelCase ->
      Environment.lookup env variable
      >>| String.split ~on:'_'
      >>| List.map ~f:String.capitalize
      >>| String.concat
      >>| String.capitalize
    | LowerCamelCase ->
      Environment.lookup env variable
      >>| String.split ~on:'_'
      >>| List.map ~f:String.capitalize
      >>| String.concat
      >>| String.uncapitalize
    | UpperSnakeCase -> Environment.lookup env variable >>| camel_to_snake >>| String.uppercase
    | LowerSnakeCase -> Environment.lookup env variable >>| camel_to_snake >>| String.lowercase
    | External "lsif.hover" ->
      filepath
      >>= fun filepath ->
      if debug then Format.printf "File for lsif.hover lookup: %s@." filepath;
      Environment.lookup env variable
      >>= fun value ->
      Environment.lookup_range env variable
      >>= fun { match_start = { offset; _ }; _ } ->
      let source = In_channel.read_all filepath in
      (* Inefficient. *)
      if debug then Format.printf "Read filepath, source len is %d@." @@ String.length source;
      let index = Match.Offset.index ~source in
      let line, column = Match.Offset.convert_fast ~offset index in
      let line, column = line - 1, column - 1 + String.length value - 1 in
      if debug then Format.printf "Var offset:%d line:%d col:%d @." offset line column;
      External.handler ~name:"lsif.hover" ~filepath ~line ~column
    | External _ -> assert false

  let substitute ?filepath template environment =
    let replacement_content, environment', _ =
      List.fold
        template
        ~init:([], Environment.create (), 0)
        ~f:
          (fun (result, env, pos) -> function
            | Constant c -> c :: result, env, pos + String.length c
            | Hole ({ variable; pattern; _ } as h) ->
              (match substitute_kind ?filepath h environment with
               | None -> pattern :: result, env, pos + String.length variable
               | Some value ->
                 let advance = pos + String.length value in
                 let range =
                   Range.
                     { match_start = Location.{ default with offset = pos }
                     ; match_end = Location.{ default with offset = advance }
                     }
                 in
                 (* FIXME: here we should probably use pattern, or hole. We don't
                      want to substitute var x for length value if it's used as :[x]
                      vs :[x].length in the same rewrite template. This will only
                      affect the replacement values, which won't clobber the actual
                      result. *)
                 let env = Environment.add ~range env variable value in
                 value :: result, env, advance))
    in
    String.concat (List.rev replacement_content), environment'

  (** Currently dead code. Alternative to substitute that searches for hole
      patterns and uses substr_replace_all. Don't know if it's faster, need to
      test. Also appears to have a minor offset issue. *)
  let substitute' template environment =
    let vars =
      List.filter_map template ~f:(function
        | Hole { pattern; variable; offset; kind } -> Some { pattern; variable; offset; kind }
        | _ -> None)
    in
    let template_string = to_string template in
    let replacement_content, environment =
      List.fold
        vars
        ~init:(template_string, Environment.create ())
        ~f:(fun (template, env) { variable; pattern; _ } ->
          match Environment.lookup environment variable with
          | None -> template, env
          | Some value ->
            (match String.substr_index template_string ~pattern with
             | None -> template, env
             | Some offset ->
               let range =
                 Range.
                   { match_start = Location.{ default with offset }
                   ; match_end = Location.{ default with offset = offset + String.length value }
                   }
               in
               let env = Environment.add ~range env variable value in
               String.substr_replace_all template ~pattern ~with_:value, env))
    in
    replacement_content, environment
end
