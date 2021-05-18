open Core

open Test_helpers
open Comby_kernel
open Matchers

let%expect_test "custom_long_delimiters" =
  let source =
    {|
      case
        case
          block 1
        esac

        case
          block 2
        esac
      esac
    |}
  in
  let match_template = {|case :[1] esac|} in
  let rewrite_template = {|case nuked blocks esac|} in

  run (module Alpha.Bash) source match_template rewrite_template;
  [%expect_exact {|
      case nuked blocks esac
    |}];

  run (module Omega.Bash) source match_template rewrite_template;
  [%expect_exact {|
      case nuked blocks esac
    |}]


let%expect_test "custom_long_delimiters_doesn't_work_in_go" =
  let source =
    {|
      case
        case
          block 1
        esac

        case
          block 2
        esac
      esac
    |}
  in
  let match_template = {|case :[1] esac|} in
  let rewrite_template = {|case nuked blocks esac|} in

  run (module Alpha.Go) source match_template rewrite_template;
  [%expect_exact {|
      case nuked blocks esac

        case nuked blocks esac
      esac
    |}];

  run (module Omega.Go) source match_template rewrite_template;
  [%expect_exact {|
      case nuked blocks esac

        case nuked blocks esac
      esac
    |}]
