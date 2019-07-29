type t = {
  user_defined_delimiters : (string * string) list;
  escapable_string_literals : string list;
  escape_char : char;
  raw_string_literals : (string * string) list;
  comment_parser : Types.comment_kind list;
}
[@@deriving yojson]
