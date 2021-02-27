
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


open MParser_Utils


type t = {
  block_size: int;         (** Size of a block in chars. *)
  block_overlap: int;      (** Overlap between blocks in chars. *)
  min_rspace: int;         (** Minimum space for regexp matching. *)
  length: int;             (** Length of the stream in chars. *)
  input: in_channel;       (** Input if created from a channel. *)
  buffer: Bytes.t;         (** The block buffer. *)
  mutable buffer_pos: int; (** Stream position of the current block. *)
}


(** [read_block s pos length] reads a block of [length] characters from
    current position in the input channel and writes it to the block buffer
    starting at [pos].

    The functions in this module use [read_block] exclusively to fill the
    block buffer. If the input channel is not modified after creating the
    char stream from it, there will be at least [length] characters left in
    the channel when [read_block] is called, but this condition cannot be
    enforced by this module. Therefore, an exception is raised if less than
    [length] characters are available.

    @raise Failure if less than [length] characters could be read.
*)
let read_block s pos length =
  if IO.input s.input s.buffer pos length <> length then
    failwith "MParser_Char_Stream.read_block: I/O error"

let from_string str =
  let length = String.length str in
  {
    block_size = length;
    block_overlap = 0;
    min_rspace = 0;
    length;
    input = Obj.magic 0;
    buffer = Bytes.of_string str;
    buffer_pos = 0;
  }

let from_channel ?(block_size = 1048576) ?block_overlap ?min_rspace input =

  let block_overlap =
    match block_overlap with
      | Some x -> x
      | None -> block_size / 16
  in

  let min_rspace =
    match min_rspace with
      | Some x -> x
      | None -> block_size / 64
  in

  if block_size < 1 || block_size > Sys.max_string_length then
    invalid_arg "MParser_Char_Stream.from_channel: invalid block size";
  if block_overlap < 1 || block_overlap > block_size / 2 then
    invalid_arg "MParser_Char_Stream.from_channel: invalid block overlap";
  if min_rspace < 1 || min_rspace > block_overlap then
    invalid_arg "MParser_Char_Stream.from_channel: invalid minimum rspace";

  let length = in_channel_length input in
  let block_size = min block_size length in
  let buffer = Bytes.create block_size in
  let buffer_pos = pos_in input in

  let s =
    {
      block_size;
      block_overlap;
      min_rspace;
      length;
      input;
      buffer;
      buffer_pos;
    }
  in
  read_block s 0 block_size;
  s

let length s =
  s.length

let is_valid_pos s pos =
  pos >= 0 && pos < s.length

let is_visible s pos =
  pos >= s.buffer_pos && pos < s.buffer_pos + s.block_size

(** [perform_unsafe_seek s pos] sets the position in the input stream to
    [pos], unconditionally reads the corresponding block from the input
    channel, and writes it to the block buffer. [pos] must be a valid
    position in the input channel. The function ensures that the block buffer
    contains at least [(max 0 (min (pos - 1) s.block_overlap))] characters
    from the input before [pos] and at least [(max 0 (min (length s - (pos +
    1)) s.block_overlap))] characters from the input after [pos].
*)
let perform_unsafe_seek s pos =
  let new_buffer_pos =
    min (s.length - s.block_size) (max 0 (pos - s.block_overlap))
  in
  let offset = new_buffer_pos - s.buffer_pos in
  if offset > 0 && offset < s.block_size then
    let overlap = s.block_size - offset in
    Bytes.unsafe_blit s.buffer (s.block_size - overlap) s.buffer 0 overlap;
    seek_in s.input (new_buffer_pos + overlap);
    read_block s overlap (s.block_size - overlap)
  else if offset < 0 && -offset < s.block_size then
    let overlap = s.block_size + offset in
    Bytes.unsafe_blit s.buffer 0 s.buffer (s.block_size - overlap) overlap;
    seek_in s.input new_buffer_pos;
    read_block s 0 (s.block_size - overlap)
  else
    (seek_in s.input new_buffer_pos;
     read_block s 0 s.block_size);

  s.buffer_pos <- new_buffer_pos

(** [unsafe_seek s pos] sets the position in the input stream to [pos]. If
    this position is currently not visible, i.e., not covered by the block
    buffer, it reads the corresponding block from the input channel and writes
    it to the block buffer using [perform_unsafe_seek s pos]. [pos] must be a
    valid position in the input channel.
*)
let unsafe_seek s pos =
  if not (is_visible s pos) then
    perform_unsafe_seek s pos

let seek s pos =
  if not (is_valid_pos s pos) then
    invalid_arg "MParser_Char_Stream.seek: invalid stream position";
  unsafe_seek s pos

let chars_left s pos =
  if is_valid_pos s pos then
    length s - pos
  else
    0

let read_char s pos =
  if not (is_valid_pos s pos) then
    None
  else
    (unsafe_seek s pos;
     Some (Bytes.unsafe_get s.buffer (pos - s.buffer_pos)))

let read_string s pos maxlen =
  if not (is_valid_pos s pos) then
    ""
  else
    let sub: Bytes.t =
      let len = min maxlen (chars_left s pos) in
      if is_visible s pos && is_visible s (pos + len - 1) then
        Bytes.sub s.buffer (pos - s.buffer_pos) len
      else if len <= s.block_overlap then
        (perform_unsafe_seek s pos;
         Bytes.sub s.buffer (pos - s.buffer_pos) len)
      else
        (let result = Bytes.create len in
         let chars_left = ref len in
         seek_in s.input pos;
         while !chars_left > 0 do
           let nchars = min s.block_size !chars_left in
           read_block s 0 nchars;
           Bytes.unsafe_blit s.buffer 0 result (len - !chars_left) nchars;
           chars_left := !chars_left - nchars;
         done;
         result)
    in
    Bytes.unsafe_to_string sub

let match_char s pos c =
  if not (is_valid_pos s pos) then
    false
  else
    (unsafe_seek s pos;
     c = Bytes.unsafe_get s.buffer (pos - s.buffer_pos))

let match_string s pos str =
  if not (is_valid_pos s pos) then
    str = ""
  else
    let len = String.length str in
    if len > chars_left s pos then
      false
    else if is_visible s pos && is_visible s (pos + len - 1) then
      Bytes.match_sub s.buffer (pos - s.buffer_pos) str
    else if len <= s.block_overlap then
      (perform_unsafe_seek s pos;
       Bytes.match_sub s.buffer (pos - s.buffer_pos) str)
    else
      (let result = ref true in
       let chars_left = ref len in
       seek_in s.input pos;
       while !chars_left > 0 do
         let nchars = min s.block_size !chars_left in
         read_block s 0 nchars;
         if Bytes.match_sub2 s.buffer 0 str (len - !chars_left) nchars then
           chars_left := !chars_left - nchars
         else
           (result := false;
            chars_left := 0)
       done;
       !result)


(* Regexp-related features
   -------------------------------------------------------------------------- *)

module MakeRegexp (Regexp: MParser_Sig.Regexp) = struct

  let match_regexp s pos rex =
    if not (is_valid_pos s pos) then
      None
    else
      (if not (is_visible s pos && is_visible s (pos + s.min_rspace)) then
         perform_unsafe_seek s pos;
       Regexp.exec ~rex ~pos:(pos - s.buffer_pos) s.buffer)

end
