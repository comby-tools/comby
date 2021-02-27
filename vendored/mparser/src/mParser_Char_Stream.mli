
(* MParser, a simple monadic parser combinator library
   -----------------------------------------------------------------------------
   Copyright (C) 2008, Holger Arnold
                 2014-2020, Max Mouratov

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

(** A position-based interface to character streams. *)

(** The streams are optimized for applications that mostly read a stream
    sequentially and occasionally backtrack over a bounded distance, which is
    a common usage pattern of backtracking parsers.

    The characters in a character stream provided by this module are accessed
    based on their position in the stream. A position [pos] is valid in the
    stream [s] if it satisfies [0 <= pos < length s]. Character streams can
    be created from input channels and from strings.
*)


type t
(** A character stream. *)


val from_string: string -> t
(** [from_string s] creates a character stream that contains the characters of
    the string [s]. *)

val from_channel: ?block_size:int -> ?block_overlap:int -> ?min_rspace:int -> in_channel -> t
(** [from_channel ?block_size ?block_overlap ?min_rspace chn] creates a
    character stream that contains the characters of the input channel [chn].
    The behavior of the stream is undefined if the channel is modified
    subsequently.

    When a character stream is created from an input channel, the characters
    in this channel are read in overlapping blocks, where [block_size] and
    [block_overlap] determine the size of a block and the amount of overlap.
    If the length of the channel is not greater than the block size, the whole
    channel is read at once. Otherwise, only a single block of the channel is
    kept in memory at a time. Whenever the current stream position leaves the
    part that is currently kept in memory, a new block is read from the
    channel. The channel must support seeking (i.e., must be created from a
    regular file) to enable this. If possible, blocks are read with the
    specified amount of overlap to minimize the re-reading of blocks due to
    backtracking. [min_rspace] specifies the minimum number of characters a
    regular expression is matched against (if possible) by [match_regexp].

    @param block_size default: [1048576] characters, valid range: [1 <=
    block_size <= Sys.max_string_length].

    @param block_overlap default: [block_size / 16], valid range: [1 <=
    block_overlap <= block_size / 2].

    @param min_rspace default: [block_size / 64], valid range: [1 <=
    min_rspace <= block_overlap].

    @raise Invalid_argument if the arguments are invalid. *)

val length: t -> int
(** [length s] returns the number of characters in the stream [s]. *)

val seek: t -> int -> unit
(** [seek s pos] prepares the stream for reading from position [pos]. If
    [pos] is outside the block currently held in memory, a block containing
    [pos] is read, replacing the old block.

    @raise Invalid_argument if [pos] is not a valid stream position. *)

val read_char: t -> int -> char option
(** [read_char s pos] returns [Some c] if [c] is the character at position
    [pos] in [s], or [None] if this position is not a valid position in
    [s]. *)

val read_string: t -> int -> int -> string
(** [read_string s pos maxlen] returns a string containing the next [n]
    characters in [s], where [n] is the minimum of [maxlen] and the number of
    characters remaining from position [pos]. If [pos] is not a valid
    position in [s], the empty string is returned. *)

val match_char: t -> int -> char -> bool
(** [match_char s pos c] is equivalent to [read_char s pos = Some c]. *)

val match_string: t -> int -> string -> bool
(** [match_string s pos str] is equivalent to [read_string s pos
    (String.length str) = str]. *)


(** {2 Regexp-related features} *)

module MakeRegexp (Regexp: MParser_Sig.Regexp): sig

  val match_regexp: t -> int -> Regexp.t -> Regexp.substrings option
  (** [match_regexp s pos rex] matches the regular expression [rex] against the
      characters in [s] starting at position [pos]. It returns [Some
      substrings] if the match succeeds, where [substrings] contains the matched
      substrings. If the match fails or if [pos] is not a valid position in
      [s], it returns [None].

      It is not guaranteed that [rex] is matched against the complete substream
      starting at the current position. The [min_rspace] parameter of
      {!MParser_Char_Stream.from_channel} specifies the minimum number of
      characters avaliable for matching. *)

end
