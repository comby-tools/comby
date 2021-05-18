type t =
  { mutable buf : Bigstringaf.t
  ; mutable off : int
  ; mutable len : int }

let of_bigstring ~off ~len buf =
  assert (off >= 0);
  assert (Bigstringaf.length buf >= len - off);
  { buf; off; len }

let create len =
  of_bigstring ~off:0 ~len:0 (Bigstringaf.create len)

let writable_space t =
  Bigstringaf.length t.buf - t.len

let trailing_space t =
  Bigstringaf.length t.buf - (t.off + t.len)

let compress t =
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off t.buf ~dst_off:0 ~len:t.len;
  t.off <- 0

let grow t to_copy =
  let old_len = Bigstringaf.length t.buf in
  let new_len = ref old_len in
  let space = writable_space t in
  while space + !new_len - old_len < to_copy do
    new_len := (3 * !new_len) / 2
  done;
  let new_buf = Bigstringaf.create !new_len in
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off new_buf ~dst_off:0 ~len:t.len;
  t.buf <- new_buf;
  t.off <- 0

let ensure t to_copy =
  if trailing_space t < to_copy then
    if writable_space t >= to_copy
    then compress t
    else grow t to_copy

let write_pos t =
  t.off + t.len

let feed_string t ~off ~len str =
  assert (off >= 0);
  assert (String.length str >= len - off);
  ensure t len;
  Bigstringaf.unsafe_blit_from_string str ~src_off:off t.buf ~dst_off:(write_pos t) ~len;
  t.len <- t.len + len

let feed_bigstring t ~off ~len b =
  assert (off >= 0);
  assert (Bigstringaf.length b >= len - off);
  ensure t len;
  Bigstringaf.unsafe_blit b ~src_off:off t.buf ~dst_off:(write_pos t) ~len;
  t.len <- t.len + len

let feed_input t = function
  | `String    s -> feed_string    t ~off:0 ~len:(String     .length s) s
  | `Bigstring b -> feed_bigstring t ~off:0 ~len:(Bigstringaf.length b) b

let shift t n =
  assert (t.len >= n);
  t.off <- t.off + n;
  t.len <- t.len - n

let for_reading { buf; off; len } =
  Bigstringaf.sub ~off ~len buf

module Unconsumed = struct
  type t =
    { buf : Bigstringaf.t
    ; off : int
    ; len : int }
end

let unconsumed ?(shift=0) { buf; off; len } =
  assert (len >= shift);
  { Unconsumed.buf; off = off + shift; len = len - shift }

let of_unconsumed { Unconsumed.buf; off; len } =
  { buf; off; len }

type unconsumed = Unconsumed.t =
  { buf : Bigstringaf.t
  ; off : int
  ; len : int }
