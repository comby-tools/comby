open Core_kernel

type t =
  { nested : bool
  }

let of_rule (rule : Ast.t) : t =
  List.fold rule ~init:{ nested = false } ~f:(fun acc -> function
      | Option name when String.(name = Syntax.option_nested) -> { nested = true }
      | _ -> acc)
