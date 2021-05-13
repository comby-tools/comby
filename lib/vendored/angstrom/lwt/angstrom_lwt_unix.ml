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

open Angstrom.Buffered
open Lwt

let default_pushback () = return_unit

let rec buffered_state_loop pushback state in_chan bytes =
  let size = Bytes.length bytes in
  match state with
  | Partial k ->
    Lwt_io.read_into in_chan bytes 0 size
    >|= begin function
      | 0   -> k `Eof
      | len ->
        assert (len > 0);
        k (`String (Bytes.(unsafe_to_string (sub bytes 0 len))))
    end
    >>= fun state' -> pushback ()
    >>= fun ()     -> buffered_state_loop pushback state' in_chan bytes
  | state -> return state

let handle_parse_result state =
  match state_to_unconsumed state with
  | None    -> assert false
  | Some us -> us, state_to_result state

let parse ?(pushback=default_pushback) p in_chan =
  let size  = Lwt_io.buffer_size in_chan in
  let bytes = Bytes.create size in
  buffered_state_loop pushback (parse ~initial_buffer_size:size p) in_chan bytes
  >|= handle_parse_result

let with_buffered_parse_state ?(pushback=default_pushback) state in_chan =
  let size  = Lwt_io.buffer_size in_chan in
  let bytes = Bytes.create size in
  begin match state with
  | Partial _ -> buffered_state_loop pushback state in_chan bytes
  | _         -> return state
  end
  >|= handle_parse_result

let async_many e k =
  Angstrom.(skip_many (e <* commit >>| k) <?> "async_many")

let parse_many p write in_chan =
  let wait = ref (default_pushback ()) in
  let k x = wait := write x in
  let pushback () = !wait in
  parse ~pushback (async_many p k) in_chan
