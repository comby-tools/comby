open Core
open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run lang source match_template rewrite_template =
  let module User_lang = (val lang : Matchers.Matcher) in
  User_lang.first ~configuration match_template source
  |> function
  | Ok result ->
      Rewrite.all ~source ~rewrite_template [result]
      |> (fun x -> Option.value_exn x)
      |> (fun {rewritten_source; _} -> rewritten_source)
      |> print_string
  | Error _ ->
      print_string rewrite_template

let%expect_test "user_defined_language" =
  let c =
    { Syntax_config.user_defined_delimiters= [("case", "esac")]
    ; Syntax_config.escapable_string_literals= []
    ; Syntax_config.escape_char= '\\'
    ; Syntax_config.raw_string_literals= []
    ; Syntax_config.comment_parser= [Multiline ("/*", "*/"); Until_newline "//"]
    }
  in
  let user_lang = Matchers.create c in
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
      /*
      case
        ignore this
      esac
      */
      // case
      //   ignore this
      // esac
    |}
  in
  let match_template = {|case :[1] esac|} in
  let rewrite_template = {|case nuked blocks esac|} in
  run user_lang source match_template rewrite_template ;
  [%expect_exact {|
      case nuked blocks esac
      /*
      case
        ignore this
      esac
      */
      // case
      //   ignore this
      // esac
    |}]
