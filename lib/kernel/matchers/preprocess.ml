open Core_kernel

let debug =
  match Sys.getenv "DEBUG_COMBY" with
  | exception Not_found -> false
  | _ -> true

let append_rule (module Parser : Types.Rule.S) rule parent_rule =
  let open Option in
  let rule =
    rule
    >>| Parser.create
    >>| function
    | Ok rule -> rule
    | Error e -> failwith @@ "Could not parse rule for alias entry:"^(Error.to_string_hum e)
  in
  match parent_rule, rule with
  | Some parent_rule, Some rule -> Some (parent_rule @ rule)
  | None, Some rule -> Some rule
  | Some parent_rule, None -> Some parent_rule
  | None, None -> None

let map_template (module Parser : Types.Rule.S) template pattern match_template rule parent_rule =
  let template' = String.substr_replace_all template ~pattern ~with_:match_template in
  if debug then Format.printf "Substituted: %s@." template';
  let rule' = append_rule (module Parser) rule parent_rule in
  template', rule'

let map_aliases
    (module Metasyntax : Metasyntax.S)
    (module External : External.S)
    template
    parent_rule =
  let module Parser = Rule.Make (Metasyntax) (External) in
  List.fold Metasyntax.aliases
    ~init:(template, parent_rule)
    ~f:(fun (template, parent_rule) Types.Metasyntax.{ pattern; match_template; rule } ->
        let template', parent_rule' =
          match String.substr_index template ~pattern with
          | None -> template, parent_rule
          | Some _ -> map_template (module Parser) template pattern match_template rule parent_rule
        in
        template', parent_rule')
