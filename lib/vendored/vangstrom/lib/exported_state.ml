type 'a state =
  | Partial of 'a partial
  | Done    of int * 'a
  | Fail    of int * string list * string

and 'a partial =
  { committed : int
  ; continue  : Bigstringaf.t -> off:int -> len:int -> More.t -> 'a state }


let state_to_option x = match x with
  | Done(_, v) -> Some v
  | Fail _     -> None
  | Partial _  -> None

let fail_to_string marks err =
  String.concat " > " marks ^ ": " ^ err

let state_to_result x = match x with
  | Done(_, v)          -> Ok v
  | Partial _           -> Error "incomplete input"
  | Fail(_, marks, err) -> Error (fail_to_string marks err)
