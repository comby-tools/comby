open Core
open Comby_kernel

open Matchers

open Matchers.Alpha

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run ?(configuration = configuration) (module M : Matchers.Matcher.S) source match_template rewrite_template =
  M.all ~configuration ~template:match_template ~source ()
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

  run (module Generic) source match_template rewrite_template;
  [%expect_exact {|1280|}]

let%expect_test "parse_rust_apostrophe_ok" =
  let source = {|pub struct GlobBuilder<'a> {}|} in
  let match_template = {|{}|} in
  let rewrite_template = {|{} // success|} in

  run (module Rust) source match_template rewrite_template;
  [%expect_exact {|pub struct GlobBuilder<'a> {} // success|}]

let%expect_test "parse_ocaml_apostrophe_ok" =
  let source = {|type 'a t = Poly of 'a | Int of int |} in
  let match_template = {|type :[v] t = :[_] Int of :[i]|} in
  let rewrite_template = {|:[v], :[i]|} in

  run (module OCaml) source match_template rewrite_template;
  [%expect_exact {|'a, int |}]

let%expect_test "strict_nested_matching" =
  let source = {|({})|} in
  let match_template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in

  run (module Dyck) source match_template rewrite_template;
  [%expect_exact {|{}|}]

let%expect_test "strict_nested_matching" =
  let source = {|(})|} in
  let match_template = {|(:[1])|} in
  let rewrite_template = {|:[1]|} in

  run (module Dyck) source match_template rewrite_template;
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

  run (module OCaml) source match_template rewrite_template;
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

  run (module OCaml) source match_template rewrite_template;
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

  run (module Ruby) source match_template rewrite_template;
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

  run (module Erlang) source match_template rewrite_template;
  [%expect_exact {|Big =  <rest>-> if X > 10 -> true; true -> false end</rest>.|}]

let%expect_test "ruby_blocks" =
  let source = {|class x end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>x</1>|}]

let%expect_test "ruby_blocks_1" =
  let source = {|class class def body1 end def body2 end end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>class def body1 end def body2 end end</1>|}]

let%expect_test "ruby_blocks_2" =
  let source = {|class class (def body1 end) (def body2 end) end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>class (def body1 end) (def body2 end) end</1>|}]

let%expect_test "ruby_blocks_3" =
  let source = {| def (def b end)(def b end) end |} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {| <1>(def b end)(def b end)</1> |}]

let%expect_test "ruby_blocks_4" =
  let source = {| def (def a end) f (def b end)end |} in
  let match_template = {|def :[1]end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {| <1>(def a end) f (def b end)</1> |}]

(* this is correct: there's no space in the source 'end', so it matches the inner def/end blocks *)
let%expect_test "ruby_blocks_5" =
  let source = {| def (def a end) f (def b end) end |} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {| <1>(def a end) f (def b end)</1> |}]

let%expect_test "ruby_blocks_5" =
  let source = {| def(df b ed) (df b ed)end |} in
  let match_template = {|def:[1]end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {| <1>(df b ed) (df b ed)</1> |}]

let%expect_test "ruby_blocks_5" =
  let source = {|class class ((def (x) end) f (def body end)) end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>class ((def (x) end) f (def body end)) end</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class class (def body1 end) (def body2 end) end end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|class class (<1>body1</1>) (<1>body2</1>) end end|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class class (def body1 end) (def body2 end) end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>class (def body1 end) (def body2 end) end</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class class def (def body1 end) (def body2 end) end end end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|class class <1>(def body1 end) (def body2 end)</1> end end|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class class def () (def body2 end) end end end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|class class <1>() (def body2 end)</1> end end|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def (def end) (def end) end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>(def end) (def end)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def (def end) a (def end) end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>(def end) a (def end)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def (defa aend) (adef aend) end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>(defa aend) (adef aend)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def (defa aend) a (adef aend) end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>(defa aend) a (adef aend)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def (adef a endq) end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>(adef a endq)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def adef a endq end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>adef a endq</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def def foo end end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>def foo end</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def def end endq|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|No matches.|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def adef a endq end |} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>adef a endq</1> |}]

let%expect_test "ruby_blocks_5" =
  let source = {|def fadef a qendq end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>fadef a qendq</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def defa aend end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>defa aend</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|(adef a endq)|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|No matches.|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def adef a endq end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>adef a endq</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def (adef a endq) end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>(adef a endq)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class (def ( body )end) end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>(def ( body )end)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class def ( body )end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>def ( body )end</1>|}]


let%expect_test "ruby_blocks_5" =
  let source = {|class def( body ) end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>def( body ) end</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class def() end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>def() end</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class def () end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>def () end</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class def( body )end end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>def( body )end</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class ( def( body )end) end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>( def( body )end)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|class (def( body )end) end|} in
  let match_template = {|class :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>(def( body )end)</1>|}]

let%expect_test "ruby_blocks_5" =
  let source = {|(def a endq)|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|No matches.|}]

let%expect_test "ruby_blocks_5" =
  let source = {|def  end|} in
  let match_template = {|def :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|No matches.|}]

let%expect_test "ruby_blocks_5" =
  let source = {|(def foo end)|} in
  let match_template = {|(def :[1] end)|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module Ruby) source match_template rewrite_template;
  [%expect_exact {|<1>foo</1>|}]

let%expect_test "ocaml_struct_end" =
  let source = {|= struct Something end|} in
  let match_template = {|= struct :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module OCaml) source match_template rewrite_template;
  [%expect_exact {|<1>Something</1>|}]

let%expect_test "ocaml_struct_end_2" =
  let source = {|= struct include Something end|} in
  let match_template = {|= struct :[1] end|} in
  let rewrite_template = {|<1>:[1]</1>|} in

  run (module OCaml) source match_template rewrite_template;
  [%expect_exact {|<1>include Something</1>|}]
