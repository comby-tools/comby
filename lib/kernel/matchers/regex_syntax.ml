(* The regular expression language accepted by regex holes, :[x~regex].

   This parses Perl-style syntax into a [Re.t], so that the language comby
   accepts is defined here rather than by whichever engine runs it. The engine
   is an automaton, so anything that requires backtracking is rejected with an
   explanation: lookaround, backreferences, recursion, and possessive or atomic
   matching.

   Matching is byte-oriented and character classes are ASCII-only. [Re]'s own
   classes treat bytes as Latin-1, which would split UTF-8 sequences: \w+ on
   "café" would match the first byte of the é. The exception is \b and \B, which
   are built into [Re] and do use its Latin-1 notion of a word character. *)

open Vangstrom

exception Invalid of string

(* Internal: carries the reason a pattern was rejected out of the parser. *)
exception Reject of string

let reject message = return () >>= fun () -> raise (Reject message)

(** Character sets are predicates, which makes union, complement and case
    folding trivial. They only become a [Re.t] once they are complete. *)
type charset = char -> bool

let of_charset (set : charset) =
  let rec ranges acc code =
    if code > 255 then
      List.rev acc
    else if not (set (Char.chr code)) then
      ranges acc (code + 1)
    else (
      let rec last_in_range hi =
        if hi < 255 && set (Char.chr (hi + 1)) then last_in_range (hi + 1) else hi
      in
      let hi = last_in_range code in
      ranges (Re.rg (Char.chr code) (Char.chr hi) :: acc) (hi + 1))
  in
  Re.alt (ranges [] 0)

let complement (set : charset) : charset = fun c -> not (set c)
let union (sets : charset list) : charset = fun c -> List.exists (fun set -> set c) sets
let single expected : charset = Char.equal expected
let range lo hi : charset = fun c -> Char.compare lo c <= 0 && Char.compare c hi <= 0
let digit = range '0' '9'
let lower = range 'a' 'z'
let upper = range 'A' 'Z'
let alpha = union [ lower; upper ]
let alnum = union [ alpha; digit ]
let word = union [ alnum; single '_' ]
let space = union [ range '\t' '\r'; single ' ' ]
let horizontal = union [ single ' '; single '\t' ]
let vertical = range '\n' '\r'
let graph = range '!' '~'

let posix_class = function
  | "alnum" -> Some alnum
  | "alpha" -> Some alpha
  | "ascii" -> Some (range '\x00' '\x7f')
  | "blank" -> Some horizontal
  | "cntrl" -> Some (union [ range '\x00' '\x1f'; single '\x7f' ])
  | "digit" -> Some digit
  | "graph" -> Some graph
  | "lower" -> Some lower
  | "print" -> Some (range ' ' '~')
  | "punct" -> Some (fun c -> graph c && not (alnum c))
  | "space" -> Some space
  | "upper" -> Some upper
  | "word" -> Some word
  | "xdigit" -> Some (union [ digit; range 'a' 'f'; range 'A' 'F' ])
  | _ -> None

let shorthand_class = function
  | 'd' -> Some digit
  | 'D' -> Some (complement digit)
  | 'w' -> Some word
  | 'W' -> Some (complement word)
  | 's' -> Some space
  | 'S' -> Some (complement space)
  | 'h' -> Some horizontal
  | 'H' -> Some (complement horizontal)
  | 'v' -> Some vertical
  | 'V' -> Some (complement vertical)
  | _ -> None

let control_escape = function
  | 'a' -> Some '\x07'
  | 'e' -> Some '\x1b'
  | 'f' -> Some '\x0c'
  | 'n' -> Some '\n'
  | 'r' -> Some '\r'
  | 't' -> Some '\t'
  | _ -> None

type flags =
  { caseless : bool
  ; multiline : bool
  ; dotall : bool
  ; extended : bool
  }

let fold_case flags (set : charset) : charset =
  if flags.caseless then
    fun c -> set c || set (Char.lowercase_ascii c) || set (Char.uppercase_ascii c)
  else
    set

let is_hex = union [ digit; range 'a' 'f'; range 'A' 'F' ]
let is_octal = range '0' '7'

let up_to limit predicate =
  scan_string 0 (fun count c -> if count < limit && predicate c then Some (count + 1) else None)

let char_of_code ~prefix digits =
  match int_of_string_opt (prefix ^ digits) with
  | Some code when code <= 255 -> return (Char.chr code)
  | Some _ -> reject "character codes above 255 are not supported"
  | None -> return '\x00'

(* After \x: \x41, \x{41}, or \x alone, which is NUL. *)
let hex_escape =
  peek_char
  >>= function
  | Some '{' ->
    advance 1 *> take_while is_hex
    >>= fun digits ->
    peek_char
    >>= (function
    | Some '}' when digits <> "" -> advance 1 *> char_of_code ~prefix:"0x" digits
    | _ -> reject "expected hex digits and a closing } in \\x{...}")
  | _ -> up_to 2 is_hex >>= char_of_code ~prefix:"0x"

let octal_escape = up_to 3 is_octal >>= char_of_code ~prefix:"0o"

let control_char =
  peek_char
  >>= function
  | Some c -> advance 1 *> return (Char.chr (Char.code (Char.uppercase_ascii c) lxor 0x40))
  | None -> reject "\\c at end of pattern"

let unsupported_escape = function
  | '1' .. '9' | 'g' | 'k' -> "backreferences are not supported"
  | 'p' | 'P' | 'X' -> "Unicode properties are not supported"
  | c -> Format.sprintf "unknown escape \\%c" c

(** An item in a character class. Only a single character can bound a range. *)
type class_item =
  | Literal of char
  | Set of charset

let class_escape =
  peek_char
  >>= function
  | None -> reject "missing ] to close character class"
  | Some c when is_octal c -> octal_escape >>| fun c -> Literal c
  | Some c ->
    advance 1
    *>
    (match shorthand_class c, control_escape c with
     | Some set, _ -> return (Set set)
     | None, Some literal -> return (Literal literal)
     | None, None ->
       (match c with
        | 'b' -> return (Literal '\x08')
        | 'x' -> hex_escape >>| fun c -> Literal c
        | 'c' -> control_char >>| fun c -> Literal c
        | 'Q' -> reject "\\Q is not supported in a character class"
        | c when alnum c -> reject (unsupported_escape c)
        | c -> return (Literal c)))

(* The '[:' has been consumed. *)
let posix_bracket =
  option false (char '^' *> return true)
  >>= fun negated ->
  take_while alpha
  >>= fun name ->
  option false (string ":]" *> return true)
  >>= fun closed ->
  match closed, posix_class name with
  | true, Some set -> return (if negated then complement set else set)
  | _ -> reject (Format.sprintf "unknown POSIX class [:%s" name)

let class_item =
  peek_string 2
  <|> return ""
  >>= function
  | "[:" -> advance 2 *> posix_bracket >>| fun set -> Set set
  | "[=" | "[." -> reject "POSIX collating elements are not supported"
  | _ ->
    any_char
    <|> reject "missing ] to close character class"
    >>= (function
    | '\\' -> class_escape
    | c -> return (Literal c))

(* The '[' has been consumed. A ] straight after the opening is a literal. *)
let character_class flags =
  option false (char '^' *> return true)
  >>= fun negated ->
  let rec items acc ~first =
    peek_char
    >>= function
    | None -> reject "missing ] to close character class"
    | Some ']' when not first -> advance 1 *> return acc
    | Some _ ->
      class_item
      >>= (function
      | Set set -> items (set :: acc) ~first:false
      | Literal lo ->
        peek_string 2
        <|> return ""
        >>= fun next ->
        if String.length next = 2 && next.[0] = '-' && next.[1] <> ']' then
          advance 1 *> class_item
          >>= function
          | Literal hi when Char.compare hi lo < 0 ->
            reject (Format.sprintf "range out of order in character class: %c-%c" lo hi)
          | Literal hi -> items (range lo hi :: acc) ~first:false
          | Set set -> items (set :: single '-' :: single lo :: acc) ~first:false
        else
          items (single lo :: acc) ~first:false)
  in
  items [] ~first:true
  >>| fun sets ->
  let set = fold_case flags (union sets) in
  of_charset (if negated then complement set else set)

let group_name ~closing =
  take_while word
  >>= fun name ->
  peek_char
  >>= function
  | Some c when Char.equal c closing && name <> "" && not (digit name.[0]) -> advance 1
  | _ -> reject "invalid group name"

let is_flag_char c = alpha c || Char.equal c '-'

let apply_flags flags spelled =
  let set flags enabled = function
    | 'i' -> Ok { flags with caseless = enabled }
    | 'm' -> Ok { flags with multiline = enabled }
    | 's' -> Ok { flags with dotall = enabled }
    | 'x' -> Ok { flags with extended = enabled }
    | c -> Error (Format.sprintf "unsupported flag (?%c)" c)
  in
  let rec go flags enabled i =
    if i = String.length spelled then
      Ok flags
    else if spelled.[i] = '-' then
      go flags false (i + 1)
    else (
      match set flags enabled spelled.[i] with
      | Ok flags -> go flags enabled (i + 1)
      | Error _ as error -> error)
  in
  go flags true 0

(** What one step of the sequence parser produced. Inline flags like (?i) have
    no atom of their own, and instead apply to the rest of the enclosing group,
    alternatives included. *)
type element =
  | Atoms of Re.t list (* last atom first, since a quantifier binds to it alone. *)
  | Set_flags of flags

let literal flags c = if flags.caseless then of_charset (fold_case flags (single c)) else Re.char c

let rec skip_ignored flags =
  if not flags.extended then
    return ()
  else
    peek_char
    >>= function
    | Some c when space c -> advance 1 *> skip_ignored flags
    | Some '#' -> take_till (Char.equal '\n') *> skip_ignored flags
    | _ -> return ()

(* {n}, {n,} or {n,m}. Anything else starting with { is a literal, as in PCRE. *)
let bounds =
  let number =
    take_while1 digit
    >>= fun digits ->
    match int_of_string_opt digits with
    | Some n when n <= 65535 -> return n
    | _ -> reject "repetition count is too large"
  in
  char '{' *> number
  >>= fun low ->
  choice
    [ (char '}' >>| fun _ -> low, Some low)
    ; (string ",}" >>| fun _ -> low, None)
    ; (char ',' *> number <* char '}' >>| fun high -> low, Some high)
    ]
  >>= function
  | low, Some high when high < low ->
    reject (Format.sprintf "numbers out of order in {%d,%d}" low high)
  | bounds -> return bounds

(* Laziness or possessiveness of the quantifier that was just parsed. *)
let quantifier_mode repeated =
  peek_char
  >>= function
  | Some '?' -> advance 1 *> return (Re.non_greedy repeated)
  | Some '+' ->
    reject "possessive quantifiers are not supported (they require backtracking control)"
  | _ -> return (Re.greedy repeated)

(* a{2}{3} is (a{2}){3}, unlike a**, which is an error. *)
let rec further_bounds atom =
  option None (bounds >>| Option.some)
  >>= function
  | None -> return atom
  | Some (low, high) -> quantifier_mode (Re.repn atom low high) >>= further_bounds

let quantified flags atom =
  skip_ignored flags *> peek_char
  >>= function
  | Some '*' -> advance 1 *> quantifier_mode (Re.rep atom) >>= further_bounds
  | Some '+' -> advance 1 *> quantifier_mode (Re.rep1 atom) >>= further_bounds
  | Some '?' -> advance 1 *> quantifier_mode (Re.opt atom) >>= further_bounds
  | Some '{' -> further_bounds atom
  | _ -> return atom

let one atom = Atoms [ atom ]

let escape flags =
  any_char
  <|> reject "\\ at end of pattern"
  >>= fun c ->
  match shorthand_class c, control_escape c with
  | Some set, _ -> return (one (of_charset set))
  | None, Some literal_char -> return (one (literal flags literal_char))
  | None, None ->
    (match c with
     | 'N' -> return (one Re.notnl)
     | 'R' -> return (one (Re.alt [ Re.str "\r\n"; of_charset vertical ]))
     | 'b' -> return (one (Re.alt [ Re.bow; Re.eow ]))
     | 'B' -> return (one Re.not_boundary)
     | 'A' -> return (one Re.bos)
     | 'Z' -> return (one Re.leol)
     | 'z' -> return (one Re.eos)
     | 'G' -> return (Atoms [])
     | 'E' -> return (Atoms [])
     | 'Q' ->
       (* \Q..\E, where the closing \E is optional. A quantifier that follows
          binds to the last character. *)
       let rec quoted acc =
         peek_string 2
         <|> return ""
         >>= function
         | "\\E" -> advance 2 *> return acc
         | _ ->
           peek_char
           >>= (function
           | None -> return acc
           | Some c -> advance 1 *> quoted (literal flags c :: acc))
       in
       quoted [] >>| fun atoms -> Atoms atoms
     | 'x' -> hex_escape >>| fun c -> one (literal flags c)
     | 'c' -> control_char >>| fun c -> one (literal flags c)
     | '0' -> octal_escape >>| fun c -> one (literal flags c)
     | '1' .. '7' ->
       (* Three octal digits, like \101, are a character. Otherwise it's a backreference. *)
       peek_string 2
       <|> return ""
       >>= fun rest ->
       if String.length rest = 2 && is_octal rest.[0] && is_octal rest.[1] then
         advance 2 *> char_of_code ~prefix:"0o" (String.make 1 c ^ rest)
         >>| fun c -> one (literal flags c)
       else
         reject (unsupported_escape c)
     | c when alnum c -> reject (unsupported_escape c)
     | c -> return (one (literal flags c)))

let rec alternation flags =
  sequence flags []
  >>= fun (first, flags) ->
  peek_char
  >>= function
  | Some '|' -> advance 1 *> alternation flags >>| fun rest -> Re.alt [ first; rest ]
  | _ -> return first

and sequence flags acc =
  skip_ignored flags *> peek_char
  >>= function
  | None | Some ('|' | ')') -> return (Re.seq (List.rev acc), flags)
  | Some _ ->
    element flags
    >>= (function
    | Set_flags flags -> sequence flags acc
    | Atoms [] -> sequence flags acc
    | Atoms (last :: rest) ->
      quantified flags last >>= fun last -> sequence flags ((last :: rest) @ acc))

and element flags =
  any_char
  >>= function
  | '.' -> return (one (if flags.dotall then Re.any else Re.notnl))
  | '^' -> return (one (if flags.multiline then Re.bol else Re.bos))
  | '$' -> return (one (if flags.multiline then Re.eol else Re.leol))
  | '[' -> character_class flags >>| one
  | '\\' -> escape flags
  | '(' -> group flags
  | '*' | '+' | '?' -> reject "quantifier does not follow a repeatable item"
  | c -> return (one (literal flags c))

and group flags =
  peek_char
  >>= function
  | Some '?' -> advance 1 *> extended_group flags
  | Some '*' -> reject "backtracking verbs like (*VERB) are not supported"
  | _ -> group_body flags

and group_body flags =
  alternation flags
  >>= fun body ->
  peek_char
  >>= function
  | Some ')' -> advance 1 *> return (one body)
  | _ -> reject "missing ) to close group"

and extended_group flags =
  peek_string 2
  <|> peek_string 1
  <|> return ""
  >>= fun next ->
  let first = if next = "" then '\x00' else next.[0] in
  let second = if String.length next > 1 then next.[1] else '\x00' in
  match first, second with
  | ':', _ -> advance 1 *> group_body flags
  | '#', _ ->
    advance 1 *> take_till (Char.equal ')') *> peek_char
    >>= (function
    | Some ')' -> advance 1 *> return (Atoms [])
    | _ -> reject "missing ) to close comment")
  | '=', _ | '!', _ -> reject "lookahead is not supported"
  | '<', ('=' | '!') -> reject "lookbehind is not supported"
  | '<', _ -> advance 1 *> group_name ~closing:'>' *> group_body flags
  | '\'', _ -> advance 1 *> group_name ~closing:'\'' *> group_body flags
  | 'P', '<' -> advance 2 *> group_name ~closing:'>' *> group_body flags
  | 'P', '=' -> reject "backreferences are not supported"
  | 'P', '>' -> reject "recursion is not supported"
  | '>', _ -> reject "atomic groups are not supported"
  | '|', _ -> reject "branch reset groups are not supported"
  | '(', _ -> reject "conditional groups are not supported"
  | ('R' | '&' | '+' | '0' .. '9'), _ -> reject "recursion is not supported"
  | c, _ when is_flag_char c ->
    take_while is_flag_char
    >>= fun spelled ->
    (match apply_flags flags spelled with
     | Error message -> reject message
     | Ok new_flags ->
       peek_char
       >>= (function
       | Some ')' -> advance 1 *> return (Set_flags new_flags)
       | Some ':' -> advance 1 *> group_body new_flags
       | _ -> reject "malformed (?flags) group"))
  | _ -> reject "unrecognized (? group"

let pattern =
  alternation { caseless = false; multiline = false; dotall = false; extended = false }
  >>= fun parsed ->
  peek_char
  >>= function
  | Some ')' -> reject "unmatched )"
  | _ -> return parsed

let ends_with_anchor source =
  let dollar = String.length source - 1 in
  let rec backslashes_before i =
    if i >= 0 && source.[i] = '\\' then 1 + backslashes_before (i - 1) else 0
  in
  dollar >= 0 && source.[dollar] = '$' && backslashes_before (dollar - 1) mod 2 = 0

let parse source =
  match parse_string ~consume:All pattern source with
  | Ok parsed -> parsed
  | Error message -> raise (Invalid (Format.sprintf "invalid regex %S: %s" source message))
  | exception Reject message ->
    raise (Invalid (Format.sprintf "invalid regex %S: %s" source message))
