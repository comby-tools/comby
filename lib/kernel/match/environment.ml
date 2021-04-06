open Base

module Data = struct
  type t =
    { value : string
    ; range : Range.t
    }
  [@@deriving yojson, eq, sexp]
end

open Data
type data = Data.t
[@@deriving yojson, eq, sexp]

type t = (string, data, Base.String.comparator_witness) Base.Map.t

let create () : t =
  Map.empty (module String)

let vars (env : t) : string list =
  Map.keys env

let add ?(range = Range.default) (env : t) (var : string) (value : string) : t =
  Map.add env ~key:var ~data:{ value; range }
  |> function
  | `Duplicate -> env
  | `Ok env -> env

let lookup (env : t) (var : string) : string option =
  Map.find env var
  |> Option.map ~f:(fun { value; _ } -> value)

let lookup_range (env : t) (var : string) : Range.t option =
  Map.find env var
  |> Option.map ~f:(fun { range; _ } -> range)

let fold (env : t) =
  Map.fold env

let update env var value =
  Map.change env var ~f:(Option.map ~f:(fun result -> { result with value }))

let update_range env var range =
  Map.change env var ~f:(Option.map ~f:(fun result -> { result with range }))

let to_string env =
  Map.fold env ~init:"" ~f:(fun ~key:variable ~data:{ value; _ } acc ->
      Format.sprintf "%s |-> %s\n%s" variable value acc)

let furthest_match env =
  Map.fold
    env
    ~init:0
    ~f:(fun ~key:_ ~data:{ range = { match_start = { offset; _ }; _ }; _ } max ->
        Int.max offset max)

let equal env1 env2 =
  Map.equal Data.equal env1 env2

let merge env1 env2 =
  Map.merge_skewed env1 env2 ~combine:(fun ~key:_ v1 _ -> v1)

let copy env =
  fold env ~init:(create ()) ~f:(fun ~key ~data:{ value; range } env' ->
      add ~range env' key value)

let exists env key =
  Option.is_some (lookup env key)

let to_yojson env : Yojson.Safe.json =
  let s =
    Map.fold_right env ~init:[] ~f:(fun ~key:variable ~data:{value; range} acc ->
        let item =
          `Assoc
            [ ("variable", `String variable)
            ; ("value", `String value)
            ; ("range", Range.to_yojson range)
            ]
        in
        item::acc)
  in
  `List s

let of_yojson (json : Yojson.Safe.json) =
  let open Yojson.Safe.Util in
  let env = create () in
  match json with
  | `List l ->
    List.fold l ~init:env ~f:(fun env json ->
        let variable = member "variable" json |> to_string_option in
        let value = member "value" json |> to_string_option in
        let range =
          member "range" json
          |> function
          | `Null -> Some Range.default
          | json ->
            Range.of_yojson json
            |> function
            | Ok range -> Some range
            | Error _ -> None
        in
        match variable, value with
        | Some variable, Some value ->
          add env ?range variable value
        | _ ->
          env)
    |> Result.return
  | _ -> Error "Invalid JSON for environment"
