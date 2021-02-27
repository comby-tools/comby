
(* MParser, a simple monadic parser combinator library
   -----------------------------------------------------------------------------
   Copyright (C) 2008, Holger Arnold
                 2014-2020, Max Mouratov
                 2021, Rijnard van Tonder (modifications)

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in the file LICENSE.txt).
*)

(** A monadic parser combinator library. *)

(** The parser combinators provided by this module can be used to build parsers
    for context-sensitive, infinite look-ahead grammars that are reasonably
    efficient and produce good error messages due to a controlled use of
    backtracking. The performance of the resulting parsers should be
    sufficient for most applications. The parsers get their input from
    character streams provided by the {!MParser_Char_Stream} module, which means
    that it is possible to parse files up to a size of at least 1GB.

    The [MParser] module is an OCaml version of the
    {{:http://www.quanttec.com/fparsec}FParsec} library for F# by Stephan
    Tolksdorf and the
    {{:http://research.microsoft.com/users/daan/parsec.html}Parsec} library
    for Haskell by Daan Leijen. The interface of the [MParser] module is very
    similar to the interfaces of Parsec and FParsec. For this reason, we keep
    the documentation here rather terse. See the excellent documentation of
    Parsec and FParsec for more information. Parsers should be easily
    portable from these two libraries to [MParser] (although some functions
    might behave subtly different). Where the behavior of Parsec and FParsec
    differs, [MParser] generally behaves like FParsec (but there might be
    exceptions).

    A significant drawback of the implementation is that it relies on the
    standard OCaml types [char] and [string] and therefore there is
    {e currently no support for Unicode}.
*)


(** {2 Parser state}

    The state of a parser consists of the input to be parsed, the current
    position in the input, the number of the current line, the position of the
    first character of the current line in the input, and an optional user
    state. A position [p] is valid if it satisfies [0 <= p && p < l], where [l]
    is the length of the input; all other positions are invalid. Characters can
    only be read from valid positions.

    The following functions that directly access or change the parser state
    should only be used to write very low-level parsers. All other parsers
    should be composed from parser combinators (see below). *)

type 's state
(** The type of parser states. *)


val init: MParser_Char_Stream.t -> 's -> 's state
(** [init input user] returns an initial parser state using the input stream
    [input] and the initial user state [user]. *)

val is_eof: 's state -> bool
(** [is_eof s] returns [true] if the current position of [s] is not a valid
    position, and [false] otherwise. If [is_eof] returns [false], a character
    can be read from the current position. *)

val next_state: 's state -> 's state
(** [advance s] returns the state [s] with the position advanced by one
    character if the current position of [s] is a valid position. Otherwise,
    the same state is returned. This function does not register newlines. If
    the current character is a newline, [advance_state_nl] should be used
    instead. *)

val advance_state: 's state -> int -> 's state
(** [advance_state s n] returns the state [s] with the position advanced by
    [n] characters if the current position of [s] is a valid position.
    Otherwise, the same state is returned. This function does not register
    newlines. If the current character is a newline, [advance_state_nl]
    should be used instead. *)

val advance_state_nl: 's state -> int -> 's state
(** [advance_state_nl s n] returns the state [s] with the position advanced by
    [n] characters and the line counter increased by one if the current
    position of [s] is a valid position. Otherwise, the same state is
    returned. *)

val read_char: 's state -> char option
(** [read_char s] returns [Some c] where [c] is the character at the current
    position, or [None] if this position is not a valid position. *)

val read_index: 's state -> int -> char option
(** [read_index s pos] returns [Some c] where [c] is the character at the
    position [pos], or [None] if this position is not a valid position. *)

val next_char: 's state -> char option
(** [next_char s] returns [Some c] where [c] is the character after the
    current position, or [None] if this position is not a valid position. *)

val prev_char: 's state -> char option
(** [prev_char s] returns [Some c] where [c] is the character before the
    current position, or [None] if this position is not a valid position. *)

val read_string: 's state -> int -> string
(** [read_string s maxlen] returns a string containing the next [n]
    characters, where [n] is the minimum of [maxlen] and the number of
    characters remaining from the current position. If the current position
    is not a valid position, the empty string is returned. *)

val match_char: 's state -> char -> bool
(** [match_char s c] returns [true] if [c] is the character at the current
    position, and [false] otherwise. *)

val match_string: 's state -> string -> bool
(** [match_string s str] returns [true] if the input starting at the current
    position matches the string [str], and [false] otherwise. *)


(** {2 Error handling and reporting}

    When building parsers from the parser combinators and running them using
    the [parse] functions (see below), error handling and reporting is nearly
    automatic. If a parser run fails, the [parse] functions return a
    human-readable (plain English) error message that is generated from the
    labels attached to the parsers using the labelling operators [<?>] and
    [<??>].

    The following types and functions can be used for explicit creation of
    errors in parsers and for customizing the handling of errors returned by
    parser runs. For this purpose the [parse] functions also return the
    actual [error] value in the case of a failed parser run. Typical
    applications for customized error handling are the internationalization of
    error messages and the automatic processing of parse errors. *)

type pos = int * int * int
(** An input position, consisting of an index into the input, a line number,
    and a column number. *)

type error_message =
  | Unexpected_error of string
    (** An unexpected symbol occurred in the input. *)
  | Expected_error of string
    (** A symbol that was expected in the input could not be parsed. *)
  | Message_error of string
    (** An error occurred that does not fit into any other category. *)
  | Compound_error of string * error
    (** An error occurred while parsing a part of a compound. *)
  | Backtrack_error of error
    (** The parser backtracked after an error occurred. *)
  | Unknown_error
    (** An unknown error occurred. *)
(** The type of error messages returned by parsers. *)

and error =
  | Parse_error of pos * error_message list
  | No_error
(** The type of errors returned by parsers. *)


val unexpected_error: 's state -> string -> error
(** Creates an [Unexpected_error]. The argument should describe the
    unexpected symbol that occurred in the input. *)

val expected_error: 's state -> string -> error
(** Creates an [Expected_error]. The argument should describe the symbol that
    was expected but could not be parsed. *)

val message_error: 's state -> string -> error
(** Creates a [Message_error]. The argument should contain the complete error
    message. *)

val compound_error: 's state -> string -> error -> error
(** Creates a [Compound_error]. The string argument should describe the
    compound that could not be parsed; the error argument should be the
    error that caused to compound parser to fail. *)

val backtrack_error: 's state -> error -> error
(** Creates a [Backtrack_error]. The argument should be the error that caused
    the parser to backtrack. *)

val unknown_error: 's state -> error
(** Creates an [Unknown_error]. *)

val merge_errors: error -> error -> error
(** Merges two errors. The behavior of the error reporting is undefined if
    [Parse_error] values from different positions are merged. *)


(** {2 The parser type}

    To make handling of parse errors possible, the reply of a parser must not
    only indicate whether the parser has failed or succeeded, but also whether
    the parser has consumed input. When a parser is run, the general rule is
    that when it fails, alternative parsers created using the [<|>] and
    [choice] combinators are only tried if the first parser did not consume
    input. Thus by default the resulting parsers are predictive
    (non-backtracking). This behavior can be changed by using combinators
    like [attempt] and [look_ahead]. By this means the [MParser] module can
    be used to build efficient parsers for a very large class of languages
    that provide nearly automatic handling of errors, which is virtually
    impossible with full-backtracking parsers (because the position causing
    the failure cannot be determined).

    This approach to combinator parsing has been pioneered by Daan Leijen's
    {{:http://research.microsoft.com/users/daan/parsec.html}Parsec} library.
    A more detailed presentation of it can be found in the following paper:
    Daan Leijen and Erik Meijer, {e Parsec: Direct-Style Monadic Parser
    Combinators For The Real World}, Technical Report UU-CS-2001-35,
    Departement of Computer Science, Universiteit Utrecht, 2001. *)

type ('a, 's) reply =
  | Empty_failed of error
    (** The parser failed without consuming input. *)
  | Empty_ok of 'a * 's state * error
    (** The parser succeeded without consuming input. *)
  | Consumed_failed of error
    (** The parser failed after consuming input. *)
  | Consumed_ok of 'a * 's state * error
    (** The parser succeeded after consuming input. *)
(** The type of replies returned by parsers. *)

type ('a, 's) t = 's state -> ('a, 's) reply
type ('a, 's) parser = ('a, 's) t
(** The type of parsers with result type ['a] and user state type ['s]. *)


val make_ok: bool -> 'a -> 's state -> error -> ('a, 's) reply
(** [make_ok consumed result state error] returns [Empty_ok (result, state,
    error)] if [consumed = false], and [Consumed_ok (result, state, error)]
    if [consumed = true]. *)

val make_failed: bool -> error -> ('a, 's) reply
(** [make_failed consumed error] returns [Empty_failed error] if [consumed =
    false], and [Consumed_failed error] of [consumed = true]. *)

val is_consumed: ('a, 's) reply -> bool
(** [is_consumed reply] returns [true] if [reply] is [Consumed_failed] or
    [Consumed_ok], and [false] otherwise. *)

val is_empty: ('a, 's) reply -> bool
(** [is_consumed reply] returns [true] if [reply] is [Empty_failed] or
    [Empty_ok], and [false] otherwise. *)

val is_error: ('a, 's) reply -> bool
(** [is_error reply] returns [true] if [reply] is [Empty_failed] or
    [Consumed_failed], and [false] otherwise. *)

val is_ok: ('a, 's) reply -> bool
(** [is_error reply] returns [true] if [reply] is [Empty_ok] or [Consumed_ok],
    and [false] otherwise. *)

val set_error: ('a, 's) reply -> error -> ('a, 's) reply
(** [set_error reply error] returns [reply] with the error message replaced by
    [error]. *)


(** {2 Running a parser} *)

(** The result of a parser run. In the case of [Failed], it contains a
    human-readable error message. *)
type 'a result =
  | Success of 'a
  | Failed of string * error


val parse: ('a, 's) t -> MParser_Char_Stream.t -> 's -> 'a result
(** [parse p s user] runs the parser [p] on the input stream [s] using the
    initial user state [user]. *)

val parse_string: ('a, 's) t -> string -> 's -> 'a result
(** [parse_string p str user] runs the parser [p] on the input stream produced
    from the string [str] using the initial user state [user]. The stream is
    created with [MParser_Char_Stream.from_string]. *)

val parse_channel: ('a, 's) t -> in_channel -> 's -> 'a result
(** [parse_string p chn user] runs the parser [p] on the input stream produced
    from the channel [chn] using the initial user state [user]. The stream is
    created with [MParser_Char_Stream.from_channel]. *)

val parse': ('a, 's) t -> MParser_Char_Stream.t -> 's -> ('a * 's) result
(** Same as parse, but returns the user state as well. *)

val parse_string': ('a, 's) t -> string -> 's -> ('a * 's) result
(** Same as parse_string, but returns the user state as well. *)

val parse_channel': ('a, 's) t -> in_channel -> 's -> ('a * 's) result
(** Same as parse_channel, but returns the user state as well. *)


(** {2 Parser combinators}

    {e Note:} A statement of the form "parser [p] is equivalent to [q]", where
    [q] is a compound parser, means that [p] is functionally equivalent to
    [q], that is, it behaves exactly the same as [q], although it might be
    implemented differently. Using [p] is generally more efficient than using
    the compound parser [q] and should therefore be preferred. *)

val return: 'a -> ('a, 's) t
(** [return x] always succeeds with the result [x] without consuming any
    input. *)

val try_return: ('a -> 'b) -> 'a -> string -> 's state -> ('b, 's) t
(** [try_return f x msg s0] succeeds with the result [f x] without consuming
    input if [f x] does not raise an exception. Otherwise, it fails with a
    [Message_error] with error message [msg] at state [s0]. This combinator
    is useful where a result must be computed from another parser result and
    where this computation may raise an exception. *)

val try_return2: ('a -> 'b -> 'c) -> 'a -> 'b -> string -> 's state ->
  ('c, 's) t
(** A variant of [try_return] for functions with two parameters. *)

val try_return3: ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> string ->
  's state -> ('d, 's) t
(** A variant of [try_return] for functions with three parameters. *)

val fail: string -> ('a, 's) t
(** [fail msg] always fails with a [Message_error] with error message [msg].
    The [fail] parser pretends having consumed input, so that all error
    messages are overwritten. *)

val message: string -> ('a, 's) t
(** [message msg] always fails with a [Message_error] with error message [msg]
    without consuming input, so that the error message is merged with other
    errors generated for the same input position. *)

val zero: ('a, 's) t
(** [zero] always fails with an [Unknown_error] without consuming input. *)

val bind: ('a, 's) t -> ('a -> ('b, 's) t) -> ('b, 's) t
(** [p >>= f] first applies the parser [p], then applies [f] to the resulting
    value, and finally applies the resulting parser. Since the second
    parser can depend on the result of the first parser, it is possible to
    parse context-sensitive grammars. *)

val (>>=): ('a, 's) t -> ('a -> ('b, 's) t) -> ('b, 's) t
(** [p >>= f] is equivalent to [bind p f] *)

val (>>): ('a, 's) t -> ('b, 's) t -> ('b, 's) t
(** [p >> q] is equivalent to [p >>= (fun _ -> q)]. *)

val (<<): ('a, 's) t -> ('b, 's) t -> ('a, 's) t
(** [p << q] is equivalent to [p >>= (fun x -> q >> return x)]. *)

val (>>>): ('a, 's) t -> ('b, 's) t -> ('b, 's) t
(** Camlp4-compatible alternative to [>>]. *)

val (<<<): ('a, 's) t -> ('b, 's) t -> ('a, 's) t
(** Camlp4-compatible alternative to [<<]. *)

val (>>$): ('a, 's) t -> 'b -> ('b, 's) t
(** [p >>$ x] is equivalent to [p >> return x]. *)

val (>>?): ('a, 's) t -> ('b, 's) t -> ('b, 's) t
(** [p >>? q] behaves like [p >> q], but if [q] fails without consuming input,
    it backtracks and pretends not having consumed input, even if [p] has
    consumed input. *)

val (|>>): ('a, 's) t -> ('a -> 'b) -> ('b, 's) t
(** [p |>> f] is equivalent to [p >>= (fun x -> return (f x))]. *)

val pipe2: ('a, 's) t -> ('b, 's) t -> ('a -> 'b -> 'c) -> ('c, 's) t
(** A variant of [(|>>)] for functions with two parameters. *)

val pipe3: ('a, 's) t -> ('b, 's) t -> ('c, 's) t ->
  ('a -> 'b -> 'c -> 'd) -> ('d, 's) t
(** A variant of [(|>>)] for functions with three parameters. *)

val pipe4: ('a, 's) t -> ('b, 's) t -> ('c, 's) t -> ('d, 's) t ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> ('e, 's) t
(** A variant of [(|>>)] for functions with four parameters. *)

val (<|>): ('a, 's) t -> ('a, 's) t -> ('a, 's) t
(** [p <|> q] first applies [p]. If [p] fails without consuming input, it
    applies [q]. *)

val choice: ('a, 's) t list -> ('a, 's) t
(** [choice \[p1; p2; ...; pn \]] is equivalent to [p1 <|> p2 <|> ... <|> pn
    <|> zero]. *)

val attempt: ('a, 's) t -> ('a, 's) t
(** [attempt p] behaves like [p], but if [p] fails after consuming input, it
    backtracks and pretends not having consumed input. The error message of
    [p] is wrapped inside a [Backtrack_error]. *)

val (<?>): ('a, 's) t -> string -> ('a, 's) t
(** [p <?> label] attaches the label [label] to [p]. If [p] fails without
    consuming input, the error message of [p] is replaced by an
    [Expected_error] with the label [label]. *)

val (<??>): ('a, 's) t -> string -> ('a, 's) t
(** [p <??> label] behaves like [p <?> label], but if [p] fails after
    consuming input, the error message of [p] is wrapped inside a
    [Compound_error]. *)

val look_ahead: ('a, 's) t -> ('a, 's) t
(** [look_ahead p] behaves like [p], but restores the original state after
    parsing. It always returns an empty reply. *)

val followed_by: ('a, 's) t -> string -> (unit, 's) t
(** [followed_by p msg] succeeds without consuming input and returns [()] if
    [p] succeeds at the current position. Otherwise, it fails without
    consuming input and returns an [Expected_error] with error message
    [msg]. *)

val not_followed_by: ('a, 's) t -> string -> (unit, 's) t
(** [not_followed_by p msg] succeeds without consuming input and returns [()]
    if [p] does not succeed at the current position. Otherwise, it fails
    without consuming input and returns an [Unexpected_error] with error
    message [msg]. *)

val opt: 'a -> ('a, 's) t -> ('a, 's) t
(** [opt x p] is equivalent to [p <|>$ x]. *)

val option: ('a, 's) t -> ('a option, 's) t
(** [option p] is equivalent to [p >>= (fun r -> return (Some r)) <|>$
    None]. *)

val optional: ('a, 's) t -> (unit, 's) t
(** [optional p] is equivalent to [p >>$ () <|>$ ()]. *)

val try_skip: ('a, 's) t -> (bool, 's) t
(** [try_skip p] is equivalent to [p >>$ true <|>$ false]. *)

val pair: ('a, 's) t -> ('b, 's) t -> ('a * 'b, 's) t
(** [pair p q] is equivalent to [ p >>= (fun x -> q >>= (fun y -> return (x,
    y)))]. *)

val many: ('a, 's) t -> ('a list, 's) t
(** [many p] parses zero or more occurrences of [p] and returns a list of the
    results returned by [p].

    @raise Failure if [p] doesn't accept any input. *)

val many1: ('a, 's) t -> ('a list, 's) t
(** [many1 p] parses one or more occurrences of [p] and returns a list of the
    results returned by [p].

    @raise Failure if [p] doesn't accept any input. *)

val many_rev: ('a, 's) t -> ('a list, 's) t
(** [many_rev p] is equivalent to [many p |>> List.rev].

    @raise Failure if [p] doesn't accept any input. *)

val many1_rev: ('a, 's) t -> ('a list, 's) t
(** [many1_rev p] is equivalent to [many1 p |>> List.rev].

    @raise Failure if [p] doesn't accept any input. *)

val skip: ('a, 's) t -> (unit, 's) t
(** [skip p] is equivalent to [p |>> ignore]. *)

val skip_many: ('a, 's) t -> (unit, 's) t
(** [skip_many p] is equivalent to [skip (many p)].

    @raise Failure if [p] doesn't accept any input. *)

val skip_many1: ('a, 's) t -> (unit, 's) t
(** [skip_many1 p] is equivalent to [skip (many1 p)].

    @raise Failure if [p] doesn't accept any input. *)

val many_fold_left: ('a -> 'b -> 'a) -> 'a -> ('b, 's) t -> ('a, 's) t
(** [many_fold_left f a p] is equivalent to
    [many p |>> List.fold_left f a].

    @raise Failure if [p] doesn't accept any input. *)

val many1_fold_left: ('a -> 'b -> 'a) -> 'a -> ('b, 's) t -> ('a, 's) t
(** [many1_fold_left f a p] is equivalent to
    [many1 p |>> List.fold_left f a].

    @raise Failure if [p] doesn't accept any input. *)

val many_rev_fold_left: ('a -> 'b -> 'a) -> 'a -> ('b, 's) t -> ('a, 's) t
(** [many_rev_fold_left f a p] is equivalent to
    [many p |>> List.rev |>> List.fold_left f a].

    @raise Failure if [p] doesn't accept any input. *)

val many1_rev_fold_left: ('a -> 'b -> 'a) -> 'a -> ('b, 's) t -> ('a, 's) t
(** [many1_rev_fold_left f a p] is equivalent to
    [many1 p |>> List.rev |>> List.fold_left f a].

    @raise Failure if [p] doesn't accept any input. *)

val chain_left: ('a, 's) t -> ('a -> 'a -> 'a, 's) t -> 'a -> ('a, 's) t
(** [chain_left p op x] parses zero or more occurrences of [p], separated by
    [op]. It returns the value obtained by the left-associative application
    of the functions returned by [op] to the results of [p]. If there are
    zero occurrences of [p], the value [x] is returned. *)

val chain_left1: ('a, 's) t -> ('a -> 'a -> 'a, 's) t -> ('a, 's) t
(** [chain_left1 p op] parses one or more occurrences of [p], separated by
    [op]. It returns the value obtained by the left-associative application
    of the functions returned by [op] to the results of [p]. *)

val chain_right: ('a, 's) t -> ('a -> 'a -> 'a, 's) t -> 'a -> ('a, 's) t
(** [chain_right p op x] parses zero or more occurrences of [p], separated by
    [op]. It returns the value obtained by the right-associative
    application of the functions returned by [op] to the results of [p]. If
    there are zero occurrences of [p], the value [x] is returned. *)

val chain_right1: ('a, 's) t -> ('a -> 'a -> 'a, 's) t -> ('a, 's) t
(** [chain_right1 p op] parses one or more occurrences of [p], separated by
    [op]. It returns the value obtained by the right-associative
    application of the functions returned by [op] to the results of [p]. *)

val count: int -> ('a, 's) t -> ('a list, 's) t
(** [count n p] parses exactly [n] occurrences of [p] and returns a list of
    the results returned by [p]. *)

val skip_count: int -> ('a, 's) t -> (unit, 's) t
(** [skip_count n p] is equivalent to [skip (count n p)]. *)

val between: ('a, 's) t -> ('b, 's) t -> ('c, 's) t -> ('c, 's) t
(** [between left right p] is equivalent to [left >> p << right]. *)

val sep_by: ('a, 's) t -> ('b, 's) t -> ('a list, 's) t
(** [sep_by p sep] parses zero or more occurrences of [p], separated by [sep].
    It returns a list of the results returned by [p]. *)

val sep_by1: ('a, 's) t -> ('b, 's) t -> ('a list, 's) t
(** [sep_by1 p sep] parses one or more occurrences of [p], separated by [sep].
    It returns a list of the results returned by [p]. *)

val sep_end_by: ('a, 's) t -> ('b, 's) t -> ('a list, 's) t
(** [sep_end_by p sep] parses zero or more occurrences of [p], separated and
    optionally ended by [sep]. It returns a list of the results returned by
    [p]. *)

val sep_end_by1: ('a, 's) t -> ('b, 's) t -> ('a list, 's) t
(** [sep_end_by1 p sep] parses one or more occurrences of [p], separated and
    optionally ended by [sep]. It returns a list of the results returned by
    [p]. *)

val end_by: ('a, 's) t -> ('b, 's) t -> ('a list, 's) t
(** [end_by p sep] parses zero or more occurrences of [p], separated and ended
    by [sep]. It returns a list of the results returned by [p]. *)

val end_by1: ('a, 's) t -> ('b, 's) t -> ('a list, 's) t
(** [end_by1 p sep] parses one or more occurrences of [p], separated and ended
    by [sep]. It returns a list of the results returned by [p]. *)

val many_until: ('a, 's) t -> ('b, 's) t -> ('a list, 's) t
(** [many_until p q] parses zero or more occurrences of [p] until [q] succeeds
    and returns a list of the results returned by [p]. It is equivalent to
    [many (not_followed_by q "" >> p) << q]. Note that [q] is parsed twice
    and should therefore not have side effects. *)

val skip_many_until: ('a, 's) t -> ('b, 's) t -> (unit, 's) t
(** [skip_many_until p q] is equivalent to [skip (many_until p q)]. *)


(** {2 Parsers accessing the parser state} *)

val get_input: (MParser_Char_Stream.t, 's) t
(** Returns the input stream. *)

val get_index: (int, 's) t
(** Returns the current index into the input. *)

val get_pos: (pos, 's) t
(** Returns the current position. *)

val register_nl: int -> int -> (unit, 's) t
(** [register_nl lines chars_after_nl] increases the line counter by [lines]
    and sets the beginning of the current line to [chars_after_nl] characters
    before the current index. *)

val set_pos: pos -> (unit, 's) t
(** Sets the current position. *)

val eof: (unit, 's) t
(** Parses the end of the input. *)


(** {2 Parsers accessing the user state} *)

val get_user_state: ('s, 's) t
(** Returns the current user state of the parser. *)

val set_user_state: 's -> (unit, 's) t
(** Sets the current user state of the parser. *)

val update_user_state: ('s -> 's) -> (unit, 's) t
(** [update_user_state f] applies [f] to the user state of the parser. *)


(** {2 Character-based parsers}

    The following specialized parsers and parser combinators work directly on
    the characters of the input stream and are therefore more efficient than
    the general combinators. Generally, the basic character and string
    parsers only consume input when they succeed. *)

val skip_nchars: int -> (unit, 's) t
(** [skip_nchars n] skips [n] characters of the input. Newlines are not
    registered. This parser never fails, even if there are less than [n]
    characters left.

    @raise Invalid_argument if [n < 0]. *)

val char: char -> (char, 's) t
(** [char c] parses the character [c] and returns it. *)

val skip_char: char -> (unit, 's) t
(** [skip_char c] is equivalent to [skip (char c)]. *)

val any_char: (char, 's) t
(** Parses any character and returns it. This parser does not register
    newlines. Use [any_char_or_nl] if the current character can be a
    newline. *)

val skip_any_char: (unit, 's) t
(** [skip_any_char] is equivalent to [skip any_char]. *)

val any_char_or_nl: (char, 's) t
(** [any_char_or_nl] is equivalent to [newline <|> any_char]. *)

val skip_any_char_or_nl: (unit, 's) t
(** [skip_any_char_or_nl] is equivalent to [skip any_char_or_nl]. *)

val peek_char: (char, 's) t
(** Returns the character at the position after the current position or fails
    if this is not a valid position. This parser does not consume input. *)

val string: string -> (string, 's) t
(** [string s] parses the string [s] and returns it. *)

val skip_string: string -> (unit, 's) t
(** [skip_string s] is equivalent to [skip (string s)]. *)

val any_string: int -> (string, 's) t
(** [any_string n] parses any string of [n] characters and returns it. Fails
    if there are less than [n] characters left in the input. *)

val many_chars: (char, 's) t -> (string, 's) t
(** [many_chars p] parses zero or more occurrences of [p] and returns a string
    of the results returned by [p].

    @raise Failure if [p] doesn't accept any input. *)

val many1_chars: (char, 's) t -> (string, 's) t
(** [many1_chars p] parses one or more occurrences of [p] and returns a string
    of the results returned by [p].

    @raise Failure if [p] doesn't accept any input. *)

val skip_many_chars: (char, 's) t -> (unit, 's) t
(** [skip_many_chars p] is equivalent to [skip (many_chars p)]. *)

val skip_many1_chars: (char, 's) t -> (unit, 's) t
(** [skip_many1_chars p] is equivalent to [skip (many1_chars p)]. *)

val many_chars_until: (char, 's) t -> (char, 's) t -> (string, 's) t
(** [many_chars_until p q] parses zero or more occurrences of [p] until [q]
    succeeds and returns a string of the results returned by [p]. It is
    equivalent to [many_chars (not_followed_by q "" >> p) << q]. Note that
    [q] is parsed twice and should therefore not have side effects. *)

val skip_many_chars_until: (char, 's) t -> (char, 's) t -> (unit, 's) t
(** [skip_many_chars_until p q] is equivalent to
    [skip (many_chars_until p q)]. *)

val satisfy: (char -> bool) -> (char, 's) t
(** [satisfy p] parses a character for which [p] returns [true] and returns
    this character. It fails with an [Unknown_error] if the character at the
    current position does not satisfy [p]. *)

val satisfy_l: (char -> bool) -> string -> (char, 's) t
(** [satisfy_l p label] is equivalent to [satisfy p <?> label]. *)

val skip_satisfy: (char -> bool) -> (unit, 's) t
(** [skip_satisfy p] is equivalent to [skip (satisfy p)]. *)

val skip_satisfy_l: (char -> bool) -> string -> (unit, 's) t
(** [skip_satisfy_l p label] is equivalent to [skip (satisfy_l p label)]. *)

val nsatisfy: int -> (char -> bool) -> (string, 's) t
(** [nsatisfy n p] parses the next [n] characters if [p] returns [true] for
    each of them. Otherwise it fails with an [Unknown_error] without
    consuming input. *)

val many_satisfy: (char -> bool) -> (string, 's) t
(** [many_satisfy p] is equivalent to [many_chars (satisfy p)]. *)

val many1_satisfy: (char -> bool) -> (string, 's) t
(** [many1_satisfy p] is equivalent to [many1_chars (satisfy p)]. *)

val skip_many_satisfy: (char -> bool) -> (unit, 's) t
(** [skip_many_satisfy p] is equivalent to [skip_many (satisfy p)]. *)

val skip_many1_satisfy: (char -> bool) -> (unit, 's) t
(** [skip_many1_satisfy p] is equivalent to [skip_many1 (satisfy p)]. *)

val next_char_satisfies: (char -> bool) -> (unit, 's) t
(** [next_char_satisfies p] succeeds without consuming input if [p] returns
    [true] for the character after the current position. Otherwise it fails
    with an [Unknown_error]. *)

val prev_char_satisfies: (char -> bool) -> (unit, 's) t
(** [prev_char_satisfies p] succeeds without consuming input if [p] returns
    [true] for the character before the current position. Otherwise it fails
    with an [Unknown_error]. *)

val any_of: string -> (char, 's) t
(** [any_of str] parses any character occurring in the string [str] and returns
    it. *)

val none_of: string -> (char, 's) t
(** [none_of str] parses any character not occurring in the string [str] and
    returns it. *)

val is_not: (char, 's) t -> (char, 's) t
(** [is_not c] parses any character that is not accepted by parser [c].
    Fails with [Unknown_error] if the character is accepted by [c]. *)

val uppercase: (char, 's) t
(** Parses an English uppercase letter and returns it. *)

val lowercase: (char, 's) t
(** Parses an English lowercase letter and returns it. *)

val letter: (char, 's) t
(** Parses an English letter and returns it. *)

val digit: (char, 's) t
(** Parses a decimal digit and returns it. *)

val hex_digit: (char, 's) t
(** Parses a hexadecimal digit and returns it. *)

val oct_digit: (char, 's) t
(** Parses an octal digit and returns it. *)

val alphanum: (char, 's) t
(** Parses an English letter or a decimal digit and returns it. *)

val tab: (char, 's) t
(** Parses a tab character (['\t']) and returns it. *)

val blank: (char, 's) t
(** Parses a space or a tab character ([' '] or ['\t'] and returns it. *)

val newline: (char, 's) t
(** Parses a newline (['\n'], ['\r'], or the sequence ['\r', '\n']). If it
    succeeds, it always returns ['\n']. The position in the parser state is
    correctly updated. *)

val space: (char, 's) t
(** Parses a space ([' ']), a tab (['\t']) or a newline (['\n'], ['\r'], or
    the sequence ['\r', '\n']). If a newline is parsed, it returns ['\n'] and
    correctly updates the position in the parser state. Otherwise it returns
    the parsed character. *)

val non_space: (char, 's) t
(** [non_space] is equivalent to [is_not space], with a better error message. *)

val spaces: (unit, 's) t
(** [spaces] is equivalent to [skip_many_chars space]. *)

val spaces1: (unit, 's) t
(** [spaces] is equivalent to [skip_many_chars1 space]. *)


(** {2 Expression parser} *)

type assoc =
  | Assoc_none        (** None-associative operator. *)
  | Assoc_left        (** Left-associative operator. *)
  | Assoc_right       (** Right-associative operator. *)
(** The associativity of an operator. An operator [(#)] is left-associative
    if [a # b # c = (a # b) # c], right-associative if [a # b # c = a # (b #
    c)], and non-associative if applying [(#)] to an expression with head
    operator [(#)] is not allowed. Note that a value of this type specifies
    only how an expression like [a # b # c] is parsed, not how it is
    interpreted semantically. *)

type ('a, 's) operator =
  | Infix of (('a -> 'a -> 'a, 's) t * assoc)   (** Infix operator. *)
  | Prefix of ('a -> 'a, 's) t                  (** Prefix operator. *)
  | Postfix of ('a -> 'a, 's) t                 (** Postfix operator. *)
(** The type of operators on type ['a]. The function returned by the parser
    argument to the [Infix], [Prefix], and [Postfix] constructor is used to
    build the result of applying the operator to its operands. *)


val expression: (('a, 's) operator list) list -> ('a, 's) t -> ('a, 's) t
(** [expression operators term] parses any well-formed expression that can
    built from the basic terms parsed by [term] and the operators specified in
    the operator table [operators]. The operator table is a list of
    [operator] lists that is ordered in descending precedence. All elements
    in one list of [operators] have the same precedence, but may have
    different associativities.

    Adjacent prefix and postfix operators of the same precedence are not
    well-formed. For example, if [(-)] denotes prefix negation, [--x] is not
    a well-formed expression (if [(--)] does not denote an operator on its
    own). If a prefix and a postfix operator of the same precedence are
    applied to an expression, the prefix operator is applied before the
    postfix operator.

    The following example demonstrates the usage of the [expression] parser.
    It implements a minimalistic calculator that can be used to evaluate
    expressions like [eval "(1 + 2 * 3) / -2"], which returns [-3].
    {[
    open MParser
    open Tokens

    exception Syntax_error

    let infix sym f assoc = Infix  (skip_symbol sym >> return f, assoc)
    let prefix sym f = Prefix (skip_symbol sym >> return f)

    let negate x = -x

    let operators =
    [
      [ prefix "-" negate ];
      [ infix "*" ( * ) Assoc_left; infix "/" ( / ) Assoc_left ];
      [ infix "+" ( + ) Assoc_left; infix "-" ( - ) Assoc_left ];
    ]

    let rec term s = (parens expr <|> decimal) s

    and expr s = expression operators term s

    let eval s =
      match parse_string expr s () with
        | Success x -> x
        | Failed (msg, _) ->
            print_string msg;
            raise Syntax_error
    ]}
*)


(** {2 Regexp-related features} *)

module MakeRegexp (Regexp: MParser_Sig.Regexp): sig

  val match_regexp: 's state -> Regexp.t -> Regexp.substrings option
  (** [match_regexp s rex] matches the regular expression [rex] against the
      input. It returns [Some substrings] if the match succeeds, where
      [substrings] contains the matched substrings. If the match fails or if
      the current position is already behind the last position of the input, it
      returns [None].

      If the input is read from a (large) file, [rex] is not necessarily matched
      against the complete remaining substring. The minimum number of
      characters that are guaranteed to be used for matching is specified when
      creating the input character stream. See the documentation of the
      {!MParser_Char_Stream} module for more information. *)

  val make_regexp: string -> Regexp.t
  (** Creates a regular expression from a string. *)

  val regexp: Regexp.t -> (string, 's) t
  (** [regexp rex] parses any string matching the regular expression [rex] and
      returns it.

      If the input is read from a (large) file, [rex] is not necessarily matched
      against the complete remaining substring. The minimum number of
      characters that are guaranteed to be used for matching is specified when
      creating the input character stream. See the documentation of the
      {!MParser_Char_Stream} module for more information. *)

  val regexp_substrings: Regexp.t -> (string array, 's) t
  (** [regexp_substrings rex] parses any string matching the regular expression
      [rex] and returns an array containing all matched substrings.

      If the input is read from a (large) file, [rex] is not necessarily matched
      against the complete remaining substring. The minimum number of
      characters that are guaranteed to be used for matching is specified when
      creating the input character stream. See the documentation of the
      {!MParser_Char_Stream} module for more information. *)


  (** Predefined token parsers.

      This module provides parsers for tokens that are commonly used in parsing
      computer languages. All parsers in this module skip the spaces (as
      defined by the {!MParser.spaces} parser) that occur after a token. Where
      they are applied to a user-defined parser [p], however, they do not skip
      the spaces occurring after the characters parsed by [p]. For example,
      [parens p] is equivalent to [char '(' >> spaces >> p << char ')' << spaces].
  *)
  module Tokens: sig

    val symbol: string -> (string, 's) t
    (** [symbol sym] parses the literal string [sym] and returns it. *)

    val skip_symbol: string -> (unit, 's) t
    (** [skip_symbol sym] is equivalent to [skip (symbol sym)]. *)

    val parens: ('a, 's) t -> ('a, 's) t
    (** [parens p] parses [p] between parentheses ['('] and [')']. *)

    val braces: ('a, 's) t -> ('a, 's) t
    (** [braces p] parses [p] between curly braces ['{'] and ['}']. *)

    val brackets: ('a, 's) t -> ('a, 's) t
    (** [brackets p] parses [p] between angle brackets ['<'] and ['>']. *)

    val squares: ('a, 's) t -> ('a, 's) t
    (** [squares p] parses [p] between square brackets ['\['] and ['\]']. *)

    val semi: (char, 's) t
    (** Parses a semicolon [';']. *)

    val comma: (char, 's) t
    (** Parses a comma [',']. *)

    val colon: (char, 's) t
    (** Parses a colon [':']. *)

    val dot: (char, 's) t
    (** Parses a dot ['.']. *)

    val semi_sep: ('a, 's) t -> ('a list, 's) t
    (** [semi_sep p] parses zero or more occurrences of [p], separated by [';'].
        It returns a list of the results returned by [p]. *)

    val semi_sep1: ('a, 's) t -> ('a list, 's) t
    (** [semi_sep1 p] parses one or more occurrences of [p], separated by [';'].
        It returns a list of the results returned by [p]. *)

    val semi_sep_end: ('a, 's) t -> ('a list, 's) t
    (** [semi_sep_end p] parses zero or more occurrences of [p], separated and
        optionally ended by [';']. It returns a list of the results returned by
        [p]. *)

    val semi_sep_end1: ('a, 's) t -> ('a list, 's) t
    (** [semi_sep_end1 p] parses one or more occurrences of [p], separated and
        optionally ended by [';']. It returns a list of the results returned by
        [p]. *)

    val semi_end: ('a, 's) t -> ('a list, 's) t
    (** [semi_end p] parses zero or more occurrences of [p], separated and ended
        by [';']. It returns a list of the results returned by [p]. *)

    val semi_end1: ('a, 's) t -> ('a list, 's) t
    (** [semi_sep_end1 p] parses one or more occurrences of [p], separated and
        ended by [';']. It returns a list of the results returned by [p]. *)

    val comma_sep: ('a, 's) t -> ('a list, 's) t
    (** [comma_sep p] parses zero or more occurrences of [p], separated by
        [',']. It returns a list of the results returned by [p]. *)

    val comma_sep1: ('a, 's) t -> ('a list, 's) t
    (** [comma_sep1 p] parses one or more occurrences of [p], separated by
        [',']. It returns a list of the results returned by [p]. *)

    val char_literal: (char, 's) t
    (** Parses a character literal as defined in the OCaml language and returns
        the character. The literal may contain an escape sequence. *)

    val string_literal: (string, 's) t
    (** Parses a string literal as defined in the OCaml language and returns the
        string. The literal may contain escape sequences. *)

    val decimal: (int, 's) t
    (** Parses a decimal natural number and returns it as an integer value.
        Fails with a [Message_error] if the parsed number is larger than
        [max_int]. *)

    val hexadecimal: (int, 's) t
    (** Parses a hexadecimal natural number as defined in the OCaml language
        (prefixed with ["0x"] or ["0X"]) and returns it as an integer value.
        Fails with a [Message_error] if the parsed number is larger than
        [max_int]. *)

    val octal: (int, 's) t
    (** Parses an octal natural number as defined in the OCaml language
        (prefixed with ["0o"] or ["0O"]) and returns it as an integer value.
        Fails with a [Message_error] if the parsed number is larger than
        [max_int]. *)

    val binary: (int, 's) t
    (** Parses a binary natural number as defined in the OCaml language
        (prefixed with ["0b"] or ["0B"]) and returns it as an integer value.
        Fails with a [Message_error] if the parsed number is larger than
        [max_int]. *)

    val integer: (int, 's) t
    (** Parses a decimal integer number and returns its value. Fails with a
        [Message_error] if the parsed number is smaller than [min_int] or larger
        than [max_int]. *)

    val float: (float, 's) t
    (** Parses floating-point literal as defined in the OCaml language and
        returns its value. Fails with a [Message_error] if the parsed number is
        not a valid representation of a [float] value. *)

  end

end
