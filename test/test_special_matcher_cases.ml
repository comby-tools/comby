open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()


let run ?(configuration = configuration) (module M : Matchers.Matcher) source match_template rewrite_template =
  M.all ~configuration ~template:match_template ~source
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

let%expect_test "parse_rust_apostrophe_ok" =
  let source = {|width="1280"|} in
  let match_template = {|width=":[1]"|} in
  let rewrite_template = {|:[1]|} in

  run (module Matchers.Generic) source match_template rewrite_template;
  [%expect_exact {|1280|}]

let%expect_test "parse_rust_apostrophe_ok" =
  let source = {|pub struct GlobBuilder<'a> {}|} in
  let match_template = {|{}|} in
  let rewrite_template = {|{} // success|} in

  run (module Matchers.Rust) source match_template rewrite_template;
  [%expect_exact {|pub struct GlobBuilder<'a> {} // success|}]

let%expect_test "parse_ocaml_apostrophe_ok" =
  let source = {|type 'a t = Poly of 'a | Int of int |} in
  let match_template = {|type :[v] t = :[_] Int of :[i]|} in
  let rewrite_template = {|:[v], :[i]|} in

  run (module Matchers.OCaml) source match_template rewrite_template;
  [%expect_exact {|'a, int |}]

let%expect_test "strict_nested_matching" =
  let source = {|({})|} in
  let match_template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in

  run (module Matchers.Dyck) source match_template rewrite_template;
  [%expect_exact {|{}|}]

let%expect_test "strict_nested_matching" =
  let source = {|(})|} in
  let match_template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in

  run (module Matchers.Dyck) source match_template rewrite_template;
  [%expect_exact {|No matches.|}]



let%expect_test "ocaml_blocks" =
  let source = {|
    module M : sig
        type t
    end = struct
       type t = int

       module Nested_M = struct
         type r = int
       end
    end
|}
  in
  let match_template = {|struct :[1] end|} in
  let rewrite_template = {|struct <deleted> end|} in

  run (module Matchers.OCaml) source match_template rewrite_template;
  [%expect_exact {|
    module M : sig
        type t
    end = struct <deleted> end
|}]

let%expect_test "ocaml_complex_blocks_with_same_end" =
  let source = {|
    begin
    match x with
    | _ ->
        let module M = struct type t end
        begin
        begin
        match y with
        | _ -> ()
        end
        end
    end
|}
  in
  let match_template = {|begin :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Matchers.OCaml) source match_template rewrite_template;
  [%expect_exact {|
    <1>match x with
    | _ ->
        let module M = struct type t end
        begin
        begin
        match y with
        | _ -> ()
        end
        end</1>
|}]

(* FIXME(#35): "before" triggers "for" block *)
let%expect_test "ruby_blocks" =
  let source = {|
class ActionController::Base
  before_filter :generate_css_from_less

  def generate_css_from_less
    Less::More.generate_all
  end
end
|}
  in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|
<1>ActionController::Base
  before_filter :generate_css_from_less

  def generate_css_from_less
    Less::More.generate_all
  end</1>
|}]

let%expect_test "erlang_blocks" =
  let source = {|Big =  fun(X) -> if X > 10 -> true; true -> false end end.|} in
  let match_template = {|fun(:[1]) :[rest] end|} in
  let rewrite_template = {|<rest>:[rest]</rest>|} in

  run (module Matchers.Erlang) source match_template rewrite_template;
  [%expect_exact {|Big =  <rest>-> if X > 10 -> true; true -> false end</rest>.|}]

let%expect_test "ruby_blocks" =
  let source = {|class x end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Matchers.Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>x</1>|}]
