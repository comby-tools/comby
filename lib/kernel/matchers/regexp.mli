type t

(** [make pattern] compiles [pattern] to match at the current position only.

    @raise Regex_syntax.Invalid if [pattern] is not accepted. *)
val make : string -> t

(** A parser that consumes and returns what the regular expression matches at the
    current position. *)
val regexp : t -> string Vangstrom.t
