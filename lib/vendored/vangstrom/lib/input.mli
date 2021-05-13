(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

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

(** An [Input.t] represents a series of buffers, of which we only have access
    to one, and a pointer to how much has been committed, which is in the
    current buffer.

                               parser commit point
                                        V
                      +--------------------------------------+
                      |#################'####################|        current buffer
    +-----------------+--------------------------------------+-----
    |#################|#################'####################|###..   input
    +-----------------+--------------------------------------+-----
    '                 '                 '                    '
    |--------------------------------------------------------|
    '                 '          length '                    '
    |-----------------|                 '                    '
  client_committed_bytes                '                    '
    '                 '                 |--------------------|
    '                 '                parser_uncommitted_bytes
    '                 |-----------------|
    '             bytes_for_client_to_commit
    |-----------------------------------|
           parser_committed_bytes

    Note that a buffer is a subsequence of a [Bigstringaf.t], defined by [off] and [len].

    All [int] position arguments should be relative to the beginning of the
    whole input. *)

type t

val create : Bigstringaf.t -> off:int -> len:int -> committed_bytes:int -> t

val length : t -> int

val client_committed_bytes   : t -> int
val parser_committed_bytes   : t -> int
val parser_uncommitted_bytes : t -> int

val bytes_for_client_to_commit : t -> int

val unsafe_get_char     : t -> int -> char
val unsafe_get_int16_le : t -> int -> int
val unsafe_get_int32_le : t -> int -> int32
val unsafe_get_int64_le : t -> int -> int64
val unsafe_get_int16_be : t -> int -> int
val unsafe_get_int32_be : t -> int -> int32
val unsafe_get_int64_be : t -> int -> int64

val count_while : t -> int -> f:(char -> bool) -> int

val apply : t -> int -> int -> f:(Bigstringaf.t -> off:int -> len:int -> 'a) -> 'a

val commit : t -> int -> unit

val invariant : t -> unit
