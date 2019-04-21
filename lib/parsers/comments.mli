(** C-style /* */ block comment parser *)
val c_multiline : (string, _) MParser.t

(** C++-style // line comment parser *)
val c_newline : (string, _) MParser.t

(** Python-style # line comment parser *)
val python_newline : (string, _) MParser.t

val percentage_newline : (string, _) MParser.t

(** Anything until newline *)
val any_newline : string -> (string, _) MParser.t
