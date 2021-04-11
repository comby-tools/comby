open Matchers
open Match

type t = Ast.t

type result = bool * environment option

type options = Options.t

module type Engine = sig
  val options : t -> Options.t

  val sat : result -> bool

  val result_env : result -> environment option

  val apply
    :  ?matcher:(module Matcher.S)
    -> ?substitute_in_place:bool
    -> ?fresh:(unit -> string)
    -> t
    -> environment
    -> result
end
