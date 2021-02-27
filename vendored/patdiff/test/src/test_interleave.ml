open! Core
open! Async
open Import

let asexp =
  {|((matrix
   ((Wub (Doj 0.2uf Doj_min)) 0.017678127290832395)
   ((Wub (Doj 0.2uf Doj_roral)) 0.0048403389770755555)
   ((Wub (Doj 0.5uf Doj_min)) 0.014466863493201646)
   ((Wub (Doj 0.5uf Doj_roral)) 0.0032743622425625684)
   ((Wub (Doj 1uf Doj_min)) 0.0062911878828122931)
   ((Wub (Doj 1uf Doj_roral)) 0.0018302834457585553)
   ((Wub (Doj 2uf Doj_min)) 0.0005350122786104681)
   ((Wub (Doj 2uf Doj_roral)) -0.0025208962986203744)
   ((Wub (Doj 5uf Doj_min)) -0.0080637581989214044)
   ((Wub (Doj 5uf Doj_roral)) -0.00970044513736832)
|}
;;

let bsexp =
  {|((matrix
   ((Wub (Doj 0.2uf Doj_min)) 0.018567212015996211)
   ((Wub (Doj 0.2uf Doj_roral)) 0.018367577773486032)
   ((Wub (Doj 0.5uf Doj_min)) 0.010881217012225865)
   ((Wub (Doj 0.5uf Doj_roral)) 0.020752124708078318)
   ((Wub (Doj 1uf Doj_min)) 0.0082337954818188109)
   ((Wub (Doj 1uf Doj_roral)) 0.017687848509880744)
   ((Wub (Doj 2uf Doj_min)) -0.0021253502814112522)
   ((Wub (Doj 2uf Doj_roral)) 0.032593610617333894)
   ((Wub (Doj 5uf Doj_min)) -0.0029337298579622381)
   ((Wub (Doj 5uf Doj_roral)) 0.023400924901561738)
|}
;;

let%expect_test "Interleave sexp files" =
  let%bind () = patdiff ~extra_flags:[] ~prev:asexp ~next:bsexp in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,11 +1,11(off) ============================================================
    (fg:black) |(off)((matrix
    (fg:black bg:red)-|(off)   ((Wub (Doj 0.2uf Doj_min)) 0.(fg:red)017678127290832395(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 0.2uf Doj_min)) 0.(fg:green)018567212015996211(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 0.2uf Doj_roral)) 0.(fg:red)0048403389770755555(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 0.2uf Doj_roral)) 0.(fg:green)018367577773486032(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 0.5uf Doj_min)) 0.(fg:red)014466863493201646(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 0.5uf Doj_min)) 0.(fg:green)010881217012225865(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 0.5uf Doj_roral)) 0.(fg:red)0032743622425625684(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 0.5uf Doj_roral)) 0.(fg:green)020752124708078318(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 1uf Doj_min)) 0.(fg:red)0062911878828122931(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 1uf Doj_min)) 0.(fg:green)0082337954818188109(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 1uf Doj_roral)) 0.(fg:red)0018302834457585553(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 1uf Doj_roral)) 0.(fg:green)017687848509880744(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 2uf Doj_min)) 0.(fg:red)0005350122786104681(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 2uf Doj_min))(fg:green) -(off)0.(fg:green)0021253502814112522(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 2uf Doj_roral))(fg:red) -(off)0.(fg:red)0025208962986203744(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 2uf Doj_roral)) 0.(fg:green)032593610617333894(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 5uf Doj_min)) -0.(fg:red)0080637581989214044(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 5uf Doj_min)) -0.(fg:green)0029337298579622381(off))
    (fg:black bg:red)-|(off)   ((Wub (Doj 5uf Doj_roral))(fg:red) -(off)0.(fg:red)00970044513736832(off))
    (fg:black bg:green)+|(off)   ((Wub (Doj 5uf Doj_roral)) 0.(fg:green)023400924901561738(off))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
