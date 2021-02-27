
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

(** General purpose utilities. *)


module IO: sig

  val input: in_channel -> Bytes.t -> int -> int -> int
  (** [input chn b pos length] reads up to [length] characters from the
      channel [chn] and stores them in the byte-buffer [b], starting at position
      [pos]. It returns the actual number of characters read. A value less
      than [length] is only returned if there are less than [length] characters
      available from [chn] ([Pervasives.input] is allowed to read less than
      [length] characters if it "finds it convenient to do a partial read").

      @raise Invalid_argument if [pos] and [length] do not specify a valid
      substring of [b]. *)

end


module String: sig
  include module type of String

  val unique: string list -> string list
  (** [unique l] returns the sorted list of unique strings in [l]. *)

  val for_all: (char -> bool) -> string -> bool
  (** [for_all p s] returns [true] if [p c = true] for all characters [c] of
      [s], and [false] otherwise. *)

end


module Bytes: sig
  include module type of Bytes

  val match_sub: Bytes.t -> int -> string -> bool
  (** [match_sub b i s] equals [Bytes.sub b i (String.length s) = s].

      @raise Invalid_argument if [i] isn't a valid index in [b]. *)

  val match_sub2: Bytes.t -> int -> string -> int -> int -> bool
  (** [match_sub2 b i s j len] equals [Bytes.sub b i len = String.sub s j len].

      @raise Invalid_argument if [i], [j], [len] do not specify valid
      substrings of [b] and [s]. *)

end
