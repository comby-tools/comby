open Core
open Comby_kernel
open Matchers
open Test_helpers

let run (module E : Engine.S) user_lang source match_template rewrite_template =
  let (module M) = E.create user_lang in
  M.first ~configuration match_template source
  |> function
  | Ok result ->
    Rewrite.all ~source ~rewrite_template [ result ]
    |> (fun x -> Option.value_exn x)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string
  | Error _ -> print_string rewrite_template

let%expect_test "user_defined_language" =
  let user_lang =
    Language.Syntax.
      { user_defined_delimiters = [ "case", "esac" ]
      ; escapable_string_literals = None
      ; raw_string_literals = []
      ; comments = [ Multiline ("/*", "*/"); Until_newline "//" ]
      }
  in
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
  run (module Alpha) user_lang source match_template rewrite_template;
  [%expect_exact
    {|
      case nuked blocks esac
      /*
      case
        ignore this
      esac
      */
      // case
      //   ignore this
      // esac
    |}];
  run (module Omega) user_lang source match_template rewrite_template;
  [%expect_exact
    {|
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

let%expect_test "user_defined_language_from_json" =
  let json =
    {|{
      "user_defined_delimiters": [
        ["case", "esac"]
      ],
      "escapable_string_literals": {
        "delimiters": ["\""],
        "escape_character": "\\"
      },
      "raw_string_literals": [],
      "comments": [
        [ "Multiline", "/*", "*/" ],
        [ "Until_newline", "//" ]
      ]
      }
    |}
  in
  let user_lang =
    Yojson.Safe.from_string json |> Matchers.Language.Syntax.of_yojson |> Result.ok_or_failwith
  in
  let source = "" in
  let match_template = {|""|} in
  let rewrite_template = {|""|} in
  run (module Alpha) user_lang source match_template rewrite_template;
  [%expect_exact {|""|}];
  run (module Omega) user_lang source match_template rewrite_template;
  [%expect_exact {|""|}]

let%expect_test "user_defined_language_from_json_optional_escapable" =
  let json =
    {|{
      "user_defined_delimiters": [
        ["case", "esac"]
      ],
      "raw_string_literals": [],
      "comments": [
        [ "Multiline", "/*", "*/" ],
        [ "Until_newline", "//" ]
      ]
      }
    |}
  in
  let user_lang =
    Yojson.Safe.from_string json |> Matchers.Language.Syntax.of_yojson |> Result.ok_or_failwith
  in
  let source = "" in
  let match_template = {|""|} in
  let rewrite_template = {|""|} in
  run (module Alpha) user_lang source match_template rewrite_template;
  [%expect_exact {|""|}];
  run (module Omega) user_lang source match_template rewrite_template;
  [%expect_exact {|""|}]
