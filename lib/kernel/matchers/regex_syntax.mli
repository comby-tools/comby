(** The regular expression language accepted by regex holes, [:\[x~regex\]]. *)

(** Raised for a pattern that is malformed or uses an unsupported feature, with
    a message suitable for showing to the user. *)
exception Invalid of string

(** [parse pattern] parses Perl-style regular expression syntax.

    @raise Invalid if [pattern] is not accepted. *)
val parse : string -> Re.t

(** Whether [pattern] ends with a $ anchor, rather than an escaped \\$. *)
val ends_with_anchor : string -> bool
