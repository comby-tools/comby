open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let format s =
  let s = String.chop_prefix_exn ~prefix:"\n" s in
  let leading_indentation = Option.value_exn (String.lfindi s ~f:(fun _ c -> c <> ' ')) in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"
  |> String.chop_suffix_exn ~suffix:"\n"

let run ?(configuration = configuration) source match_template rewrite_template =
  Generic.first ~configuration match_template source
  |> function
  | Ok result ->
    Rewrite.all ~source ~rewrite_template [result]
    |> (fun x -> Option.value_exn x)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string
  | Error _ ->
    (* this is too annoying to fix everytime the grammar changes. *)
    print_string ""

let run_all ?(configuration = configuration) source match_template rewrite_template =
  Generic.all ~configuration ~template:match_template ~source
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string


let%expect_test "basic" =
  let source = {|a b c d|} in
  let match_template = {|:[1]|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|a b c d|}];

  let source = {|a b c d|} in
  let match_template = {|a :[1] c d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b|}];

  let source = {|a b c d|} in
  let match_template = {|a :[1] d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b c|}];

  let source = {|a b c d|} in
  let match_template = {|a :[1]|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b c d|}];

  let source = {|a b c d|} in
  let match_template = {|:[1] c d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|a b|}];

  let source = {|a b c d|} in
  let match_template = {|:[1] :[2]|} in
  let rewrite_template = {|(:[1]) (:[2])|} in
  run source match_template rewrite_template;
  [%expect_exact {|(a) (b c d)|}];

  let source = {|a b c d|} in
  let match_template = {|:[2] :[1]|} in
  let rewrite_template = {|(:[2]) (:[1])|} in
  run source match_template rewrite_template;
  [%expect_exact {|(a) (b c d)|}];

  let source = {|a b c d|} in
  let match_template = {|a :[2] :[1] d|} in
  let rewrite_template = {|(:[2]) (:[1])|} in
  run source match_template rewrite_template;
  [%expect_exact {|(b) (c)|}];

  let source = {|a b c d|} in
  let match_template = {|a :[2] :[1]|} in
  let rewrite_template = {|(:[2]) (:[1])|} in
  run source match_template rewrite_template;
  [%expect_exact {|(b) (c d)|}];

  let source = {|a b c d|} in
  let match_template = {|a :[2] c :[1]|} in
  let rewrite_template = {|(:[2]) (:[1])|} in
  run source match_template rewrite_template;
  [%expect_exact {|(b) (d)|}];

  let source = {|x:|} in
  let match_template = {|:[1]:|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|x|}]

let%expect_test "basic_failures" =
  let source = {|a x b bbq|} in
  let match_template = {|a :[1] b c|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {||}];

  let source = {|a b c d|} in
  let match_template = {|a :[2] d :[1]|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact
    {||}];

  let source = {|a b c d|} in
  let match_template = {|a :[2] b :[1]|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {||}]

let%expect_test "delimiter_matching" =
  let source = {|(a b c) d|} in
  let match_template = {|(:[1]) d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|a b c|}];

  let source = {|(a b c) d|} in
  let match_template = {|(:[1] b :[2]) d|} in
  let rewrite_template = {|(:[1]) (:[2])|} in
  run source match_template rewrite_template;
  [%expect_exact {|(a) (c)|}];

  let source = {|q(a b c) d|} in
  let match_template = {|q(:[1] b :[2]) d|} in
  let rewrite_template = {|(:[1]) (:[2])|} in
  run source match_template rewrite_template;
  [%expect_exact {|(a) (c)|}];

  let source = {|((a) b)|} in
  let match_template = {|(:[1] b)|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|(a)|}];

  let source = {|((a b c)) d|} in
  let match_template = {|(:[1]) d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|(a b c)|}];

  let source = {|((a b c)) d|} in
  let match_template = {|(:[1]) d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|(a b c)|}];

  let source = {|((a b c) q) d|} in
  let match_template = {|((:[1]) q) d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|a b c|}];

  let source = {|((a b c) q) d|} in
  let match_template = {|((:[1] c) q) d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|a b|}];

  let source = {|((a b () c) q) d|} in
  let match_template = {|((:[1] () c) q) d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|a b|}];

  let source = {|((a ((x) d) b c)) d|} in
  let match_template = {|((a :[1] :[2] c)) d|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|((x) d)|}];

  let source = {|((a ((x) d) b c)) d|} in
  let match_template = {|((a (:[1]) :[2] c)) d|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|(x) d b|}];

  let source = {|(b (c) d)|} in
  let match_template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b (c) d|}];

  let source = {|(b (c) d.)|} in
  let match_template = {|(:[1].)|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b (c) d|}];

  let source = {|(b (c.) d.)|} in
  let match_template = {|(:[1].)|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b (c.) d|}];

  let source = {|(b. (c) d.)|} in
  let match_template = {|(:[1].)|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b. (c) d|}];

  let source = {|(b (c) d.)|} in
  let match_template = {|(b :[1] d.)|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|(c)|}];

  let source = {|outer(inner(dst,src),src)|} in
  let match_template = {|outer(:[1],src)|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|inner(dst,src)|}];

  let source = {|(b ((c)) d.)|} in
  let match_template = {|(b :[1] d.)|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|((c))|}];

  let source = {|a b c|} in
  let match_template = {|a :[1] c|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|b|}];

  let source = {|x = foo;|} in
  let match_template = {|x = :[1];|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|foo|}];

  let source = {|((a {{x} d} b c)) d|} in
  let match_template = {|((a {:[1] d} :[2] c)) d|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|{x} b|}];

  let source = {|((a {([{x}]) d} b c)) d|} in
  let match_template = {|((a {:[1] d} :[2] c)) d|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|([{x}]) b|}];

  let source = {|(((((x)))))|} in
  let match_template = {|(((:[1])))|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|((x))|}];

  let source = {|((((y(x)z))))|} in
  let match_template = {|(((:[1])))|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|(y(x)z)|}];

  let source = {|((((y(x)z))))|} in
  let match_template = {|(((:[1]):[2]))|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|(y(x)z) |}];

  let source = {|(((x)z))|} in
  let match_template = {|(((:[1]):[2]))|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|x z|}];

  let source = {|((((x))z))|} in
  let match_template = {|(((:[1]):[2]))|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|(x) z|}];

  let source = {|lolwtfbbq|} in
  let match_template = {|lol:[1]bbq|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|wtf|}];

  let source = {|x = foo; x = bar;|} in
  let match_template = {|x = :[1];|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|foo x = bar;|}];

  let source = {|[ no match prefix ] x = foo; [ no match suffix ]|} in
  let match_template = {|x = :[1];|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|[ no match prefix ] foo [ no match suffix ]|}];

  let source = {|x = a; x = b; x = c|} in
  let match_template = {|x = :[1];|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|a x = b; x = c|}];

  let source = {|x = ( x = x; );|} in
  let match_template = {|x = :[1];|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|( x = x; )|}];

  let source = {|( x = x = x; )|} in
  let match_template = {|x = :[1];|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|( x = x )|}];

  let source = {|xxx a b d c 1 2 3 b d d blah|} in
  let match_template = {|a :[1] c :[2] d|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|xxx b d 1 2 3 b d blah|}];

  let source = {|howevenlolwtfbbqispossible|} in
  let match_template = {|lol:[1]bbq|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|howevenwtfispossible|}];

  let source = {|lolhowevenlolwtfbbqispossiblebbq|} in
  let match_template = {|lol:[1]bbq|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|howevenlolwtfispossiblebbq|}];

  let source = {|hello my name is bob the builder|} in
  let match_template = {|:[alongidentifiername] :[2] :[3] :[xyz] :[5] :[6]|} in
  let rewrite_template = {|:[alongidentifiername] :[2] :[3] :[xyz] :[5] :[6]|} in
  run source match_template rewrite_template;
  [%expect_exact {|hello my name is bob the builder|}];

  let source = {|www.testdofooname.com/picsinsideit/stunningpictureofkays1381737242g8k4n-280x428.jpg|} in
  let match_template = {|www.:[1]-:[2].jpg|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|testdofooname.com/picsinsideit/stunningpictureofkays1381737242g8k4n 280x428|}];

  let source = {|https://api.github.com/repos/dmjacobsen/slurm/commits/716c1499695c68afcab848a1b49653574b4fc167|} in
  let match_template = {|:[1]api.:[2]/repos/:[3]s/:[4]|} in
  let rewrite_template = {|:[1] :[2] :[3] :[4]|} in
  run source match_template rewrite_template;
  [%expect_exact {|https:// github.com dmjacobsen/slurm/commit 716c1499695c68afcab848a1b49653574b4fc167|}];

  let source =
    {|
      assert(stream->md_len + md_len -
             si.foo_data_begin <= MAD_BUFFER_MDLEN);
      memcpy(*stream->foo_data + stream->md_len,
             mad_bit_nextbyte(&stream->ptr),
             frame_used = md_len - si.foo_data_begin);
      stream->md_len += frame_used;
    |} |> format
  in
  let match_template = {|memcpy(:[1], :[2], :[3]);|} in
  let rewrite_template = {|:[1], :[2], :[3]|} in
  run source match_template rewrite_template;
  [%expect_exact {|assert(stream->md_len + md_len -
       si.foo_data_begin <= MAD_BUFFER_MDLEN);
*stream->foo_data + stream->md_len, mad_bit_nextbyte(&stream->ptr), frame_used = md_len - si.foo_data_begin
stream->md_len += frame_used;|}]

let%expect_test "significant_whitespace" =
  let configuration = Configuration.create ~match_kind:Fuzzy ~significant_whitespace:true () in
  let run = run ~configuration in

  let source = {|two  spaces|} in
  let match_template = {|:[1]  :[2]|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|two spaces|}];

  (* FIXME: this should fail. also test case where separators do or do not need
     whitespace.  e.g., strict about strcpy(src,dst) matching a template
     strcpy(:[1],:[2]) versus strcpy(:[1], :[2]) *)
  let source = {|two  spaces|} in
  let match_template = {|:[1] :[2]|} in
  let rewrite_template = {|:[1] :[2]|} in
  run source match_template rewrite_template;
  [%expect_exact {|two spaces|}]

let%expect_test "contextual_matching" =
  let run = run_all in

  let source = {|memcpy(dst1, src1, 1); memcpy(dst2, src2, 2);|} in
  let match_template = {|memcpy(:[1], :[2], :[3])|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|dst1; dst2;|}];

  let source = {|memcpy(dst1, src1, 1); memcpy(dst2, src2, 2);|} in
  let match_template = {|memcpy(:[1], :[2], :[3])|} in
  let rewrite_template = {|:[1]|} in
  run source match_template rewrite_template;
  [%expect_exact {|dst1; dst2;|}]

let%expect_test "contextual_matching_with_short_hole_syntax" =
  let run = run_all in

  let source = {|memcpy(dst1, src1, 1); memcpy(dst2, src2, 2);|} in
  let match_template = {|memcpy(:[[1]], :[2], :[3])|} in
  let rewrite_template = {|:[[1]]|} in
  run source match_template rewrite_template;
  [%expect_exact {|dst1; dst2;|}]

let%expect_test "trivial_empty_case" =
  let source = "" in
  let match_template = "" in
  begin
    Generic.all ~configuration ~template:match_template ~source
    |> function
    | [] -> print_string "No matches."
    | hd :: _ ->
      print_string (Yojson.Safe.to_string (Match.to_yojson hd))
  end;
  [%expect_exact {|{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":0,"line":1,"column":1}},"environment":[],"matched":""}|}]
