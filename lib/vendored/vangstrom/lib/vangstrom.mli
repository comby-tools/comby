(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

(** Parser combinators built for speed and memory-efficiency.

    Angstrom is a parser-combinator library that provides monadic and
    applicative interfaces for constructing parsers with unbounded lookahead.
    Its parsers can consume input incrementally, whether in a blocking or
    non-blocking environment. To achieve efficient incremental parsing,
    Angstrom offers both a buffered and unbuffered interface to input streams,
    with the {!module:Unbuffered} interface enabling zero-copy IO. With these
    features and low-level iteration parser primitives like {!take_while} and
    {!skip_while}, Angstrom makes it easy to write efficient, expressive, and
    reusable parsers suitable for high-performance applications. *)


type +'a t
(** A parser for values of type ['a]. *)


type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** {2 Basic parsers} *)

val peek_char : char option t
(** [peek_char] accepts any char and returns it, or returns [None] if the end
    of input has been reached.

    This parser does not advance the input. Use it for lookahead. *)

val peek_char_fail : char t
(** [peek_char_fail] accepts any char and returns it. If end of input has been
    reached, it will fail.

    This parser does not advance the input. Use it for lookahead. *)

val peek_string : int -> string t
(** [peek_string n] accepts exactly [n] characters and returns them as a
    string. If there is not enough input, it will fail.

    This parser does not advance the input. Use it for lookahead. *)

val char : char -> char t
(** [char c] accepts [c] and returns it. *)

val not_char : char -> char t
(** [not_char] accepts any character that is not [c] and returns the matched
    character. *)

val any_char : char t
(** [any_char] accepts any character and returns it. *)

val satisfy : (char -> bool) -> char t
(** [satisfy f] accepts any character for which [f] returns [true] and
    returns the accepted character. In the case that none of the parser
    succeeds, then the parser will fail indicating the offending
    character. *)

val string : string -> string t
(** [string s] accepts [s] exactly and returns it. *)

val string_ci : string -> string t
(** [string_ci s] accepts [s], ignoring case, and returns the matched string,
    preserving the case of the original input. *)

val skip : (char -> bool) -> unit t
(** [skip f] accepts any character for which [f] returns [true] and discards
    the accepted character. [skip f] is equivalent to [satisfy f] but discards
    the accepted character. *)

val skip_while : (char -> bool) -> unit t
(** [skip_while f] accepts input as long as [f] returns [true] and discards
    the accepted characters. *)

val take : int -> string t
(** [take n] accepts exactly [n] characters of input and returns them as a
    string. *)

val take_while : (char -> bool) -> string t
(** [take_while f] accepts input as long as [f] returns [true] and returns the
    accepted characters as a string.

    This parser does not fail. If [f] returns [false] on the first character,
    it will return the empty string. *)

val take_while1 : (char -> bool) -> string t
(** [take_while1 f] accepts input as long as [f] returns [true] and returns the
    accepted characters as a string.

    This parser requires that [f] return [true] for at least one character of
    input, and will fail otherwise. *)

val take_till : (char -> bool) -> string t
(** [take_till f] accepts input as long as [f] returns [false] and returns the
    accepted characters as a string.

    This parser does not fail. If [f] returns [true] on the first character, it
    will return the empty string. *)

val consumed : _ t -> string t
(** [consumed p] runs [p] and returns the contents that were consumed during the
    parsing as a string *)

val take_bigstring : int -> bigstring t
(** [take_bigstring n] accepts exactly [n] characters of input and returns them
    as a newly allocated bigstring. *)

val take_bigstring_while : (char -> bool) -> bigstring t
(** [take_bigstring_while f] accepts input as long as [f] returns [true] and
    returns the accepted characters as a newly allocated bigstring.

    This parser does not fail. If [f] returns [false] on the first character,
    it will return the empty bigstring. *)

val take_bigstring_while1 : (char -> bool) -> bigstring t
(** [take_bigstring_while1 f] accepts input as long as [f] returns [true] and
    returns the accepted characters as a newly allocated bigstring.

    This parser requires that [f] return [true] for at least one character of
    input, and will fail otherwise. *)

val take_bigstring_till : (char -> bool) -> bigstring t
(** [take_bigstring_till f] accepts input as long as [f] returns [false] and
    returns the accepted characters as a newly allocated bigstring.

    This parser does not fail. If [f] returns [true] on the first character, it
    will return the empty bigstring. *)

val consumed_bigstring : _ t -> bigstring t
(** [consumed p] runs [p] and returns the contents that were consumed during the
    parsing as a bigstring *)

val advance : int -> unit t
(** [advance n] advances the input [n] characters, failing if the remaining
    input is less than [n]. *)

val end_of_line : unit t
(** [end_of_line] accepts either a line feed [\n], or a carriage return
    followed by a line feed [\r\n] and returns unit. *)

val at_end_of_input : bool t
(** [at_end_of_input] returns whether the end of the end of input has been
    reached. This parser always succeeds. *)

val end_of_input : unit t
(** [end_of_input] succeeds if all the input has been consumed, and fails
    otherwise. *)

val scan : 'state -> ('state -> char -> 'state option) -> (string * 'state) t
(** [scan init f] consumes until [f] returns [None]. Returns the final state
    before [None] and the accumulated string *)

val scan_state : 'state -> ('state -> char -> 'state option) -> 'state t
(** [scan_state init f] is like {!scan} but only returns the final state before
    [None]. Much more efficient than {!scan}. *)

val scan_string : 'state -> ('state -> char -> 'state option) -> string t
(** [scan_string init f] is like {!scan} but discards the final state and returns
    the accumulated string. *)

val int8 : int -> int t
(** [int8 i] accepts one byte that matches the lower-order byte of [i] and
    returns unit. *)

val any_uint8 : int t
(** [any_uint8] accepts any byte and returns it as an unsigned int8. *)

val any_int8 : int t
(** [any_int8] accepts any byte and returns it as a signed int8. *)

(** Big endian parsers *)
module BE : sig
  val int16 : int   -> unit t
  (** [int16 i] accept two bytes that match the two lower order bytes of [i]
      and returns unit. *)

  val int32 : int32 -> unit t
  (** [int32 i] accept four bytes that match the four bytes of [i]
      and returns unit. *)

  val int64 : int64 -> unit t
  (** [int64 i] accept eight bytes that match the eight bytes of [i] and
      returns unit. *)

  val any_int16 : int t
  val any_int32 : int32 t
  val any_int64 : int64 t
  (** [any_intN] reads [N] bits and interprets them as big endian signed integers. *)

  val any_uint16 : int t
  (** [any_uint16] reads [16] bits and interprets them as a big endian unsigned
      integer. *)

  val any_float : float t
  (** [any_float] reads 32 bits and interprets them as a big endian floating
      point value. *)

  val any_double : float t
  (** [any_double] reads 64 bits and interprets them as a big endian floating
      point value. *)
end

(** Little endian parsers *)
module LE : sig
  val int16 : int   -> unit t
  (** [int16 i] accept two bytes that match the two lower order bytes of [i]
      and returns unit. *)

  val int32 : int32 -> unit t
  (** [int32 i] accept four bytes that match the four bytes of [i]
      and returns unit. *)

  val int64 : int64 -> unit t
  (** [int32 i] accept eight bytes that match the eight bytes of [i] and
      returns unit. *)

  val any_int16 : int t
  val any_int32 : int32 t
  val any_int64 : int64 t
  (** [any_intN] reads [N] bits and interprets them as little endian signed
      integers. *)

  val any_uint16 : int t
  (** [uint16] reads [16] bits and interprets them as a little endian unsigned
      integer. *)

  val any_float : float t
  (** [any_float] reads 32 bits and interprets them as a little endian floating
      point value. *)

  val any_double : float t
  (** [any_double] reads 64 bits and interprets them as a little endian floating
      point value. *)
end


(** {2 Combinators} *)

val option : 'a -> 'a t -> 'a t
(** [option v p] runs [p], returning the result of [p] if it succeeds and [v]
    if it fails. *)


val both : 'a t -> 'b t -> ('a * 'b) t
(** [both p q] runs [p] followed by [q] and returns both results in a tuple *)

val list : 'a t list -> 'a list t
(** [list ps] runs each [p] in [ps] in sequence, returning a list of results of
    each [p]. *)

val count : int -> 'a t -> 'a list t
(** [count n p] runs [p] [n] times, returning a list of the results. *)

val many : 'a t -> 'a list t
(** [many p] runs [p] {i zero} or more times and returns a list of results from
    the runs of [p]. *)

val many1 : 'a t -> 'a list t
(** [many1 p] runs [p] {i one} or more times and returns a list of results from
    the runs of [p]. *)

val many_till : 'a t -> _ t -> 'a list t
(** [many_till p e] runs parser [p] {i zero} or more times until action [e]
    succeeds and returns the list of result from the runs of [p]. *)

val sep_by : _ t -> 'a t -> 'a list t
(** [sep_by s p] runs [p] {i zero} or more times, interspersing runs of [s] in between. *)

val sep_by1 : _ t -> 'a t -> 'a list t
(** [sep_by1 s p] runs [p] {i one} or more times, interspersing runs of [s] in between. *)

val not_followed_by : _ t -> unit t
(** [not_followed_by p] succeeds when [p] does not succeed at the current
     position. Otherwise, it fails. It does not consume input. *)

val skip_many : _ t -> unit t
(** [skip_many p] runs [p] {i zero} or more times, discarding the results. *)

val skip_many1 : _ t -> unit t
(** [skip_many1 p] runs [p] {i one} or more times, discarding the results. *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] computes the fixpoint of [f] and runs the resultant parser. The
    argument that [f] receives is the result of [fix f], which [f] must use,
    paradoxically, to define [fix f].

    [fix] is useful when constructing parsers for inductively-defined types
    such as sequences, trees, etc. Consider for example the implementation of
    the {!many} combinator defined in this library:

{[let many p =
  fix (fun m ->
    (cons <$> p <*> m) <|> return [])]}

    [many p] is a parser that will run [p] zero or more times, accumulating the
    result of every run into a list, returning the result. It's defined by
    passing [fix] a function. This function assumes its argument [m] is a
    parser that behaves exactly like [many p]. You can see this in the
    expression comprising the left hand side of the alternative operator
    [<|>]. This expression runs the parser [p] followed by the parser [m], and
    after which the result of [p] is cons'd onto the list that [m] produces.
    The right-hand side of the alternative operator provides a base case for
    the combinator: if [p] fails and the parse cannot proceed, return an empty
    list.

    Another way to illustrate the uses of [fix] is to construct a JSON parser.
    Assuming that parsers exist for the basic types such as [false], [true],
    [null], strings, and numbers, the question then becomes how to define a
    parser for objects and arrays? Both contain values that are themselves JSON
    values, so it seems as though it's impossible to write a parser that will
    accept JSON objects and arrays before writing a parser for JSON values as a
    whole.

    This is the exact situation that [fix] was made for. By defining the
    parsers for arrays and objects within the function that you pass to [fix],
    you will gain access to a parser that you can use to parse JSON values, the
    very parser you are defining!

{[let json =
  fix (fun json ->
    let arr = char '[' *> sep_by (char ',') json <* char ']' in
    let obj = char '{' *> ... json ... <* char '}' in
    choice [str; num; arr json, ...])]} *)


(** {2 Alternatives} *)

val (<|>) : 'a t -> 'a t -> 'a t
(** [p <|> q] runs [p] and returns the result if succeeds. If [p] fails, then
    the input will be reset and [q] will run instead. *)

val choice : ?failure_msg:string -> 'a t list -> 'a t
(** [choice ?failure_msg ts] runs each parser in [ts] in order until one
    succeeds and returns that result. In the case that none of the parser
    succeeds, then the parser will fail with the message [failure_msg], if
    provided, or a much less informative message otherwise. *)

val (<?>) : 'a t -> string -> 'a t
(** [p <?> name] associates [name] with the parser [p], which will be reported
    in the case of failure. *)

val commit : unit t
(** [commit] prevents backtracking beyond the current position of the input,
    allowing the manager of the input buffer to reuse the preceding bytes for
    other purposes.

    The {!module:Unbuffered} parsing interface will report directly to the
    caller the number of bytes committed to the when returning a
    {!Unbuffered.state.Partial} state, allowing the caller to reuse those bytes
    for any purpose. The {!module:Buffered} will keep track of the region of
    committed bytes in its internal buffer and reuse that region to store
    additional input when necessary. *)


(** {2 Monadic/Applicative interface} *)

val return : 'a -> 'a t
(** [return v] creates a parser that will always succeed and return [v] *)

val fail : string -> _ t
(** [fail msg] creates a parser that will always fail with the message [msg] *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [p >>= f] creates a parser that will run [p], pass its result to [f], run
    the parser that [f] produces, and return its result. *)

val bind : 'a t -> f:('a -> 'b t) -> 'b t
(** [bind] is a prefix version of [>>=] *)

val (>>|) : 'a t -> ('a -> 'b) -> 'b t
(** [p >>| f] creates a parser that will run [p], and if it succeeds with
    result [v], will return [f v] *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** [f <*> p] is equivalent to [f >>= fun f -> p >>| f]. *)

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
(** [f <$> p] is equivalent to [p >>| f] *)

val ( *>) : _ t -> 'a t -> 'a t
(** [p *> q] runs [p], discards its result and then runs [q], and returns its
    result. *)

val (<* ) : 'a t -> _ t -> 'a t
(** [p <* q] runs [p], then runs [q], discards its result, and returns the
    result of [p]. *)

val lift : ('a -> 'b) -> 'a t -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
(** The [liftn] family of functions promote functions to the parser monad.
    For any of these functions, the following equivalence holds:

{[liftn f p1 ... pn = f <$> p1 <*> ... <*> pn]}

    These functions are more efficient than using the applicative interface
    directly, mostly in terms of memory allocation but also in terms of speed.
    Prefer them over the applicative interface, even when the arity of the
    function to be lifted exceeds the maximum [n] for which there is an
    implementation for [liftn]. In other words, if [f] has an arity of [5] but
    only [lift4] is provided, do the following:

{[lift4 f m1 m2 m3 m4 <*> m5]}

    Even with the partial application, it will be more efficient than the
    applicative implementation. *)

val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
val map4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t
(** The [mapn] family of functions are just like [liftn], with a slightly
    different interface. *)

(** The [Let_syntax] module is intended to be used with the [ppx_let]
    pre-processor, and just contains copies of functions described elsewhere. *)
module Let_syntax : sig
  val return : 'a -> 'a t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
    val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
    val map4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t
  end
end

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

(** Unsafe Operations on Angstrom's Internal Buffer

    These functions are considered {b unsafe} as they expose the input buffer
    to client code without any protections against modification, or leaking
    references. They are exposed to support performance-sensitive parsers that
    want to avoid allocation at all costs. Client code should take care to
    write the input buffer callback functions such that they:

    {ul
    {- do not modify the input buffer {i outside} of the range
       [\[off, off + len)];}
    {- do not modify the input buffer {i inside} of the range
       [\[off, off + len)] if the parser might backtrack; and}
    {- do not return any direct or indirect references to the input buffer.}}

    If the input buffer callback functions do not do any of these things, then
    the client may consider their use safe. *)
module Unsafe : sig

  val take : int -> (bigstring -> off:int -> len:int -> 'a) -> 'a t
  (** [take n f] accepts exactly [n] characters of input into the parser's
      internal buffer then calls [f buffer ~off ~len].  [buffer] is the
      parser's internal buffer.  [off] is the offset from the start of [buffer]
      containing the requested content.  [len] is the length of the requested
      content.  [len] is guaranteed to be equal to [n]. *)

  val take_while : (char -> bool) -> (bigstring -> off:int -> len:int -> 'a) -> 'a t
  (** [take_while check f] accepts input into the parser's interal buffer as
      long as [check] returns [true] then calls [f buffer ~off ~len].  [buffer]
      is the parser's internal buffer.  [off] is the offset from the start of
      [buffer] containing the requested content.  [len] is the length of the
      content matched by [check].

      This parser does not fail. If [check] returns [false] on the first
      character, [len] will be [0]. *)

  val take_while1 : (char -> bool) -> (bigstring -> off:int -> len:int -> 'a) -> 'a t
  (** [take_while1 check f] accepts input into the parser's interal buffer as
      long as [check] returns [true] then calls [f buffer ~off ~len].  [buffer]
      is the parser's internal buffer.  [off] is the offset from the start of
      [buffer] containing the requested content.  [len] is the length of the
      content matched by [check].

      This parser requires that [f] return [true] for at least one character of
      input, and will fail otherwise. *)

  val take_till : (char -> bool) -> (bigstring -> off:int -> len:int -> 'a) -> 'a t
  (** [take_till check f] accepts input into the parser's interal buffer as
      long as [check] returns [false] then calls [f buffer ~off ~len].  [buffer]
      is the parser's internal buffer.  [off] is the offset from the start of
      [buffer] containing the requested content.  [len] is the length of the
      content matched by [check].

      This parser does not fail. If [check] returns [true] on the first
      character, [len] will be [0]. *)

  val peek : int -> (bigstring -> off:int -> len:int -> 'a) -> 'a t
  (** [peek n ~f] accepts exactly [n] characters and calls [f buffer ~off ~len]
      with [len = n]. If there is not enough input, it will fail.

      This parser does not advance the input. Use it for lookahead. *)
end


(** {2 Running} *)

module Consume : sig
  type t =
    | Prefix
    | All
end

val parse_bigstring : consume:Consume.t -> 'a t -> bigstring -> ('a, string) result

(** [parse_bigstring ~consume t bs] runs [t] on [bs]. The parser will receive
    an [`Eof] after all of [bs] has been consumed. Passing {!Prefix} in the
    [consume] argument allows the parse to successfully complete without
    reaching eof.  To require the parser to reach eof, pass {!All} in the
    [consume] argument.

    For use-cases requiring that the parser be fed input incrementally, see the
    {!module:Buffered} and {!module:Unbuffered} modules below. *)


val parse_string : consume:Consume.t -> 'a t -> string -> ('a, string) result
(** [parse_string ~consume t bs] runs [t] on [bs]. The parser will receive an
    [`Eof] after all of [bs] has been consumed. Passing {!Prefix} in the
    [consume] argument allows the parse to successfully complete without
    reaching eof.  To require the parser to reach eof, pass {!All} in the
    [consume] argument.

    For use-cases requiring that the parser be fed input incrementally, see the
    {!module:Buffered} and {!module:Unbuffered} modules below. *)


(** Buffered parsing interface.

    Parsers run through this module perform internal buffering of input. The
    parser state will keep track of unconsumed input and attempt to minimize
    memory allocation and copying. The {!Buffered.state.Partial} parser state
    will accept newly-read, incremental input and copy it into the internal
    buffer. Users can feed parser states using the {!feed} function. As a
    result, the interface is much easier to use than the one exposed by the
    {!Unbuffered} module.

    On success or failure, any unconsumed input will be returned to the user
    for additional processing. The buffer that the unconsumed input is returned
    in can also be reused. *)
module Buffered : sig
  type unconsumed =
    { buf : bigstring
    ; off : int
    ; len : int }

  type input =
    [ `Bigstring of bigstring
    | `String    of string ]

  type 'a state =
    | Partial of ([ input | `Eof ] -> 'a state) (** The parser requires more input. *)
    | Done    of unconsumed * 'a (** The parser succeeded. *)
    | Fail    of unconsumed * string list * string (** The parser failed. *)

  val parse : ?initial_buffer_size:int -> 'a t -> 'a state
  (** [parse ?initial_buffer_size t] runs [t] and awaits input if needed.
      [parse] will allocate a buffer of size [initial_buffer_size] (defaulting
      to 4k bytes) to do input buffering and automatically grows the buffer as
      needed. *)

  val feed : 'a state -> [ input | `Eof ] -> 'a state
  (** [feed state input] supplies the parser state with more input. If [state] is
      [Partial], then parsing will continue where it left off. Otherwise, the
      parser is in a [Fail] or [Done] state, in which case the [input] will be
      copied into the state's buffer for later use by the caller. *)

  val state_to_option : 'a state -> 'a option
  (** [state_to_option state] returns [Some v] if the parser is in the
      [Done (bs, v)] state and [None] otherwise. This function has no effect on
      the current state of the parser. *)

  val state_to_result : 'a state -> ('a, string) result
  (** [state_to_result state] returns [Ok v] if the parser is in the [Done (bs, v)]
      state and [Error msg] if it is in the [Fail] or [Partial] state.

      This function has no effect on the current state of the parser. *)

  val state_to_unconsumed : _ state -> unconsumed option
  (** [state_to_unconsumed state] returns [Some bs] if [state = Done(bs, _)] or
      [state = Fail(bs, _, _)] and [None] otherwise. *)

end

(** Unbuffered parsing interface.

    Use this module for total control over memory allocation and copying.
    Parsers run through this module perform no internal buffering. Instead, the
    user is responsible for managing a buffer containing the entirety of the
    input that has yet to be consumed by the parser. The
    {!Unbuffered.state.Partial} parser state reports to the user how much input
    the parser consumed during its last run, via the
    {!Unbuffered.partial.committed} field. This area of input must be discarded
    before parsing can resume. Once additional input has been collected, the
    unconsumed input as well as new input must be passed to the parser state
    via the {!Unbuffered.partial.continue} function, together with an
    indication of whether there is {!Unbuffered.more} input to come.

    The logic that must be implemented in order to make proper use of this
    module is intricate and tied to your OS environment. It's advisable to use
    the {!Buffered} module when initially developing and testing your parsers.
    For production use-cases, consider the Async and Lwt support that this
    library includes before attempting to use this module directly. *)
module Unbuffered : sig
  type more =
    | Complete
    | Incomplete

  type 'a state =
    | Partial of 'a partial (** The parser requires more input. *)
    | Done    of int * 'a (** The parser succeeded, consuming specified bytes. *)
    | Fail    of int * string list * string (** The parser failed, consuming specified bytes. *)
  and 'a partial =
    { committed : int
      (** The number of bytes committed during the last input feeding.
          Callers must drop this number of bytes from the beginning of the
          input on subsequent calls. See {!commit} for additional details. *)
    ; continue : bigstring -> off:int -> len:int -> more -> 'a state
      (** A continuation of a parse that requires additional input. The input
          should include all uncommitted input (as reported by previous partial
          states) in addition to any new input that has become available, as
          well as an indication of whether there is {!more} input to come.  *)
    }

  val parse : 'a t -> 'a state
  (** [parse t] runs [t] and await input if needed. *)

  val state_to_option : 'a state -> 'a option

  (** [state_to_option state] returns [Some v] if the parser is in the
      [Done (bs, v)] state and [None] otherwise. This function has no effect on the
      current state of the parser. *)

  val state_to_result : 'a state -> ('a, string) result
  (** [state_to_result state] returns [Ok v] if the parser is in the
      [Done (bs, v)] state and [Error msg] if it is in the [Fail] or [Partial]
      state.

      This function has no effect on the current state of the parser. *)
end

(** {2 Expert Parsers}

    For people that know what they're doing. If you want to use them, read the
    code. No further documentation will be provided. *)

val pos : int t
val available : int t
