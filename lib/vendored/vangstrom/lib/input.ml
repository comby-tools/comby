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

type t =
  { mutable parser_committed_bytes : int
  ; client_committed_bytes         : int
  ; off                            : int
  ; len                            : int
  ; buffer                         : Bigstringaf.t
  }

let create buffer ~off ~len ~committed_bytes =
  { parser_committed_bytes = committed_bytes
  ; client_committed_bytes = committed_bytes
  ; off
  ; len
  ; buffer }

let length                 t = t.client_committed_bytes + t.len
let client_committed_bytes t = t.client_committed_bytes
let parser_committed_bytes t = t.parser_committed_bytes

let committed_bytes_discrepancy t = t.parser_committed_bytes - t.client_committed_bytes
let bytes_for_client_to_commit  t = committed_bytes_discrepancy t

let parser_uncommitted_bytes t = t.len - bytes_for_client_to_commit t

let invariant t =
  assert (parser_committed_bytes t + parser_uncommitted_bytes t = length t);
  assert (parser_committed_bytes t - client_committed_bytes   t = bytes_for_client_to_commit t);
;;

let offset_in_buffer t pos =
  t.off + pos - t.client_committed_bytes

let apply t pos len ~f =
  let off = offset_in_buffer t pos in
  f t.buffer ~off ~len

let unsafe_get_char t pos =
  let off = offset_in_buffer t pos in
  Bigstringaf.unsafe_get t.buffer off

let unsafe_get_int16_le t pos =
  let off = offset_in_buffer t pos in
  Bigstringaf.unsafe_get_int16_le t.buffer off

let unsafe_get_int32_le t pos =
  let off = offset_in_buffer t pos in
  Bigstringaf.unsafe_get_int32_le t.buffer off

let unsafe_get_int64_le t pos =
  let off = offset_in_buffer t pos in
  Bigstringaf.unsafe_get_int64_le t.buffer off

let unsafe_get_int16_be t pos =
  let off = offset_in_buffer t pos in
  Bigstringaf.unsafe_get_int16_be t.buffer off

let unsafe_get_int32_be t pos =
  let off = offset_in_buffer t pos in
  Bigstringaf.unsafe_get_int32_be t.buffer off

let unsafe_get_int64_be t pos =
  let off = offset_in_buffer t pos in
  Bigstringaf.unsafe_get_int64_be t.buffer off

let count_while t pos ~f =
  let buffer = t.buffer in
  let off    = offset_in_buffer t pos in
  let i      = ref off in
  let limit  = t.off + t.len in
  while !i < limit && f (Bigstringaf.unsafe_get buffer !i) do
    incr i
  done;
  !i - off
;;

let commit t pos =
  t.parser_committed_bytes <- pos
;;
