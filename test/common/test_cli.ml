open Core

module Time = Core_kernel.Time_ns.Span

let binary_path = "../../../../comby"

let read_with_timeout read_from_channels =
  let read_from_fds = List.map ~f:Unix.descr_of_in_channel read_from_channels in
  let read_from_channels =
    Unix.select
      ~restart:true
      ~read:read_from_fds
      ~write:[]
      ~except:[]
      ~timeout:(`After (Time.of_int_sec 1))
      ()
    |> (fun { Unix.Select_fds.read; _ } -> read)
    |> List.map ~f:Unix.in_channel_of_descr
  in
  List.map read_from_channels ~f:In_channel.input_all
  |> String.concat ~sep:"\n"

let read_output command =
  let open Unix.Process_channels in
  let { stdout; stderr; _ } =
    Unix.open_process_full ~env:(Array.of_list ["COMBY_TEST=1"]) command
  in
  let stdout_result = In_channel.input_all stdout in
  let stderr_result = In_channel.input_all stderr in
  stdout_result ^ stderr_result

let read_expect_stdin_and_stdout command source =
  let open Unix.Process_channels in
  let { stdin; stdout; stderr } =
    Unix.open_process_full ~env:(Array.of_list ["COMBY_TEST=1"]) command
  in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  let stdout_result = In_channel.input_all stdout in
  let stderr_result = In_channel.input_all stderr in
  stdout_result ^ stderr_result

let read_expect_stderr command source =
  let open Unix.Process_channels in
  let { stdin; stdout; stderr } =
    Unix.open_process_full ~env:(Array.of_list ["COMBY_TEST=1"]) command
  in
  Out_channel.output_string stdin source;
  Out_channel.flush stdin;
  Out_channel.close stdin;
  let _ = In_channel.input_all stdout in
  let stderr_result = In_channel.input_all stderr in
  stderr_result

let%expect_test "json_lines_separates_by_line" =
  let source = "hello world" in
  let match_template = "o" in
  let rewrite_template = "i" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -json-lines" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|{"uri":null,"rewritten_source":"helli wirld","in_place_substitutions":[{"range":{"start":{"offset":7,"line":-1,"column":-1},"end":{"offset":8,"line":-1,"column":-1}},"replacement_content":"i","environment":[]},{"range":{"start":{"offset":4,"line":-1,"column":-1},"end":{"offset":5,"line":-1,"column":-1}},"replacement_content":"i","environment":[]}],"diff":"--- /dev/null\n+++ /dev/null\n@@ -1,1 +1,1 @@\n-hello world\n\\ No newline at end of file\n+helli wirld\n\\ No newline at end of file"}
|}]

let%expect_test "json_lines_json_pretty_do_not_output_when_diff_null" =
  let source = "hello world" in
  let match_template = "asdf" in
  let rewrite_template = "asdf" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -json-lines" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{| |}]

let%expect_test "json_lines_do_not_output_when_diff_null" =
  let source = "hello world" in
  let match_template = "asdf" in
  let rewrite_template = "asdf" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -json-lines" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{| |}]

let%expect_test "error_on_zip_and_stdin" =
  let command_args = "-zip x -stdin" in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command "none" in
  print_string result;
  [%expect_exact {|No templates specified. See -h to specify on the command line, or use -templates <directory-containing-templates>.
Next error: -zip may not be used with -stdin.
|}]

let%expect_test "error_on_stdout_and_diff" =
  let command_args = "'' '' -stdout -diff" in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command "none" in
  print_string result;
  [%expect_exact {|-stdout may not be used with -diff. Note: -stdout outputs the changed file contents and -diff outputs a unified diff. Choose one of these.
|}]

let%expect_test "error_on_invalid_templates_dir" =
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -templates nonexistent" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|One or more directories specified with -templates is not a directory.
|}]

let%expect_test "warn_on_anonymous_and_templates_flag" =
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -templates example/templates/identity" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|WARNING: Templates specified on the command line AND using -templates. Ignoring match
      and rewrite templates on the command line and only using those in directories.
|}]


let%expect_test "stdin_command" =
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;43;30m!|[0m[0;31mhello [0mworld
|}]

let%expect_test "with_match_rule" =
  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let rule = {|where :[1] == "world"|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -rule '%s' -f .c "
      match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;43;30m!|[0m[0;31mhello [0mworld
|}];

  let source = "hello world" in
  let match_template = "hello :[1]" in
  let rewrite_template = ":[1]" in
  let rule = {|where :[1] != "world"|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -rule '%s' -f .c -stdout"
      match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    hello world |}]

let%expect_test "with_rewrite_rule" =
  let source = "hello world" in
  let match_template = ":[[2]] :[[1]]" in
  let rewrite_template = ":[1]" in
  let rule = {|where rewrite :[1] { ":[_]" -> ":[2]" }|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -rule '%s' -f .c "
      match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;43;30m!|[0mhello[0;31m world[0m
|}]

let%expect_test "with_rewrite_rule_stdin_default_no_extension" =
  let source = "hello world" in
  let match_template = ":[[2]] :[[1]]" in
  let rewrite_template = ":[1]" in
  let rule = {|where rewrite :[1] { ":[_]" -> ":[2]" }|} in
  let command_args =
    Format.sprintf "-sequential '%s' '%s' -rule '%s' -stdin" match_template rewrite_template rule
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;43;30m!|[0mhello[0;31m world[0m

WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning.
|}]

let%expect_test "generic_matcher" =
  let source = {|\footnote{\small \url{https://github.com}}|} in
  let match_template = {|\footnote{\small :[1]}|} in
  let rewrite_template = {|\footnote{\scriptsize :[1]}|} in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .generic" match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;2m\footnote{[0m[0;31m\small[0m[0;2m \url{https://github.com}}[0m[0m
[0;42;30m+|[0m[0m\footnote{[0;32m\scriptsize[0m \url{https://github.com}}[0m

WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning.
|}]


let%expect_test "json_output_option" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential -json-lines '%s' '%s' -f .c "
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|{"uri":null,"rewritten_source":"c X a c Y a","in_place_substitutions":[{"range":{"start":{"offset":6,"line":-1,"column":-1},"end":{"offset":11,"line":-1,"column":-1}},"replacement_content":"c Y a","environment":[{"variable":"1","value":"Y","range":{"start":{"offset":2,"line":-1,"column":-1},"end":{"offset":3,"line":-1,"column":-1}}}]},{"range":{"start":{"offset":0,"line":-1,"column":-1},"end":{"offset":5,"line":-1,"column":-1}},"replacement_content":"c X a","environment":[{"variable":"1","value":"X","range":{"start":{"offset":2,"line":-1,"column":-1},"end":{"offset":3,"line":-1,"column":-1}}}]}],"diff":"--- /dev/null\n+++ /dev/null\n@@ -1,1 +1,1 @@\n-a X c a Y c\n\\ No newline at end of file\n+c X a c Y a\n\\ No newline at end of file"}
|}];

  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential -json-lines -match-only '%s' '%s' -f .c "
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":5,"line":1,"column":6}},"environment":[{"variable":"1","value":"X","range":{"start":{"offset":2,"line":1,"column":3},"end":{"offset":3,"line":1,"column":4}}}],"matched":"a X c"},{"range":{"start":{"offset":6,"line":1,"column":7},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"1","value":"Y","range":{"start":{"offset":8,"line":1,"column":9},"end":{"offset":9,"line":1,"column":10}}}],"matched":"a Y c"}]}
|}]

let with_zip f =
  let file = Filename.temp_file "comby_" ".zip" in
  let zip =  Zip.open_out file in
  let entry_name = "main.ml" in
  let entry_content = "hello world" in
  Zip.add_entry entry_content zip entry_name;
  Zip.close_out zip;
  f file;
  Unix.remove file

let%expect_test "list_languages" =
  let command_args = "-list" in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_output command in
  print_string result;
  [%expect_exact {|Option              Language  
 -matcher .s        Assembly  
 -matcher .sh       Bash      
 -matcher .c        C         
 -matcher .cs       C#        
 -matcher .css      CSS       
 -matcher .dart     Dart      
 -matcher .dyck     Dyck      
 -matcher .clj      Clojure   
 -matcher .elm      Elm       
 -matcher .erl      Erlang    
 -matcher .ex       Elixir    
 -matcher .f        Fortran   
 -matcher .fsx      F#        
 -matcher .go       Go        
 -matcher .html     HTML      
 -matcher .hs       Haskell   
 -matcher .java     Java      
 -matcher .js       Javascript
 -matcher .json     JSON      
 -matcher .jl       Julia     
 -matcher .kt       Kotlin    
 -matcher .tex      LaTeX     
 -matcher .lisp     Lisp      
 -matcher .nim      Nim       
 -matcher .ml       OCaml     
 -matcher .paren    Paren     
 -matcher .pas      Pascal    
 -matcher .php      PHP       
 -matcher .py       Python    
 -matcher .re       Reason    
 -matcher .rb       Ruby      
 -matcher .rs       Rust      
 -matcher .scala    Scala     
 -matcher .sql      SQL       
 -matcher .swift    Swift     
 -matcher .txt      Text      
 -matcher .ts       Typescript
 -matcher .xml      XML       
 -matcher .generic  Generic   
|}]


let%expect_test "patdiff_and_zip" =
  with_zip (fun file ->
      let match_template = ":[[2]] :[[1]]" in
      let rewrite_template = ":[1]" in
      let command_args =
        Format.sprintf "'%s' '%s' .ml -sequential -json-lines -zip %s"
          match_template rewrite_template file
      in
      let command = Format.sprintf "%s %s" binary_path command_args in
      let result = read_output command in
      print_string result;
      [%expect_exact {|{"uri":"main.ml","rewritten_source":"world","in_place_substitutions":[{"range":{"start":{"offset":0,"line":-1,"column":-1},"end":{"offset":5,"line":-1,"column":-1}},"replacement_content":"world","environment":[{"variable":"1","value":"world","range":{"start":{"offset":0,"line":-1,"column":-1},"end":{"offset":5,"line":-1,"column":-1}}}]}],"diff":"--- main.ml\n+++ main.ml\n@@ -1,1 +1,1 @@\n-hello world\n\\ No newline at end of file\n+world\n\\ No newline at end of file"}
|}]
    )

let%expect_test "template_parsing_no_match_template" =
  let source = "hello world" in
  let template_dir = "example" ^/ "templates" ^/ "parse-no-match-template" in
  let command_args = Format.sprintf "-stdin -sequential -f .c -templates %s" template_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|WARNING: Could not read required match file in example/templates/parse-no-match-template
|}]

let%expect_test "template_parsing_with_trailing_newline" =
  let source = "hello world" in
  let template_dir = "example" ^/ "templates" ^/ "parse-template-no-trailing-newline" in
  let command_args = Format.sprintf "-stdin -sequential -f .c -templates %s -stdout" template_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    hello world |}]

let%expect_test "template_parsing_with_trailing_newline" =
  let source = "hello world" in
  let template_dir = "example" ^/ "templates" ^/ "parse-template-with-trailing-newline" in
  let command_args = Format.sprintf "-stdin -sequential -f .c -templates %s -stdout" template_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    hello world |}]

let%expect_test "nested_templates" =
  let source = "1 2 3" in
  let template_dir = "example" ^/ "multiple-nested-templates" in
  let command_args = Format.sprintf "-stdin -sequential -f .c -templates %s -stdout" template_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    +1 +2 +3WARNING: Could not read required match file in example/multiple-nested-templates/invalid-subdir |}]

let%expect_test "diff_is_default" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;31ma[0m[0;2m X [0m[0;31mc a[0m[0;2m Y [0m[0;31mc[0m[0m
[0;42;30m+|[0m[0m[0;32mc[0m X [0;32ma c[0m Y [0;32ma[0m[0m
|}]

let%expect_test "diff_option" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential -diff '%s' '%s' -f .c"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|--- /dev/null
+++ /dev/null
@@ -1,1 +1,1 @@
-a X c a Y c
\ No newline at end of file
+c X a c Y a
\ No newline at end of file
|}]

let%expect_test "stdout_option" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential -stdout '%s' '%s' -f .c"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|c X a c Y a|}]

let%expect_test "only_color_prints_colored_diff" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -color"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;31ma[0m[0;2m X [0m[0;31mc a[0m[0;2m Y [0m[0;31mc[0m[0m
[0;42;30m+|[0m[0m[0;32mc[0m X [0;32ma c[0m Y [0;32ma[0m[0m
|}]

let%expect_test "diff_explicit_color" =
  let source = "a X c a Y c" in
  let match_template = "a :[1] c" in
  let rewrite_template = "c :[1] a" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -f .c -diff -color"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;31ma[0m[0;2m X [0m[0;31mc a[0m[0;2m Y [0m[0;31mc[0m[0m
[0;42;30m+|[0m[0m[0;32mc[0m X [0;32ma c[0m Y [0;32ma[0m[0m
|}]

let%expect_test "is_real_directory" =
  let source = "hello world" in
  let src_dir = "example" ^/ "src" ^/ "main.c" in
  let command_args = Format.sprintf "'main' 'pain' -sequential -d %s -exclude-dir 'ignore' -diff" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    Directory specified with -d or -directory is not a directory. |}]

let%expect_test "exclude_dir_option" =
  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'main' 'pain' -sequential -d %s -exclude-dir 'ignore' -diff" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/honor-file-extensions/honor.pb.generic
    +++ example/src/honor-file-extensions/honor.pb.generic
    @@ -1,3 +1,3 @@
    -func main() {
    +func pain() {
     // foo()
     }
    --- example/src/honor-file-extensions/honor.pb.go
    +++ example/src/honor-file-extensions/honor.pb.go
    @@ -1,4 +1,4 @@
    -func main() {
    +func pain() {
     // in a comment foo()
     foo()
     }
    --- example/src/main.c
    +++ example/src/main.c
    @@ -1,1 +1,1 @@
    -int main() {}
    +int pain() {}

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}];

  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'main' 'pain' -sequential -d %s -exclude-dir 'nonexist' -diff" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/honor-file-extensions/honor.pb.generic
    +++ example/src/honor-file-extensions/honor.pb.generic
    @@ -1,3 +1,3 @@
    -func main() {
    +func pain() {
     // foo()
     }
    --- example/src/honor-file-extensions/honor.pb.go
    +++ example/src/honor-file-extensions/honor.pb.go
    @@ -1,4 +1,4 @@
    -func main() {
    +func pain() {
     // in a comment foo()
     foo()
     }
    --- example/src/ignore-me/main.c
    +++ example/src/ignore-me/main.c
    @@ -1,1 +1,1 @@
    -int main() {}
    +int pain() {}
    --- example/src/main.c
    +++ example/src/main.c
    @@ -1,1 +1,1 @@
    -int main() {}
    +int pain() {}

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}]

let%expect_test "dir_depth_option" =
  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'depth_' 'correct_depth_' -sequential -directory %s -depth %d -diff" src_dir (-1) in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{| -depth must be 0 or greater. |}];

  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'depth_' 'correct_depth_' -sequential -directory %s -depth %d -diff" src_dir 0 in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/depth-0.c
    +++ example/src/depth-0.c
    @@ -1,1 +1,1 @@
    -int depth_0() {}
    +int correct_depth_0() {}

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}];

  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'depth_' 'correct_depth_' -sequential -directory %s -depth %d -diff" src_dir 1 in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/depth-0.c
    +++ example/src/depth-0.c
    @@ -1,1 +1,1 @@
    -int depth_0() {}
    +int correct_depth_0() {}
    --- example/src/depth-1/depth-1.c
    +++ example/src/depth-1/depth-1.c
    @@ -1,1 +1,1 @@
    -int depth_1() {}
    +int correct_depth_1() {}

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}];

  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'depth_' 'correct_depth_' -sequential -directory %s -depth %d -diff" src_dir 2 in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/depth-0.c
    +++ example/src/depth-0.c
    @@ -1,1 +1,1 @@
    -int depth_0() {}
    +int correct_depth_0() {}
    --- example/src/depth-1/depth-1.c
    +++ example/src/depth-1/depth-1.c
    @@ -1,1 +1,1 @@
    -int depth_1() {}
    +int correct_depth_1() {}
    --- example/src/depth-1/depth-2/depth-2.c
    +++ example/src/depth-1/depth-2/depth-2.c
    @@ -1,1 +1,1 @@
    -int depth_2() {}
    +int correct_depth_2() {}

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}];

  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'depth_' 'correct_depth_' -sequential -directory %s -depth %d -diff" src_dir 1000 in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/depth-0.c
    +++ example/src/depth-0.c
    @@ -1,1 +1,1 @@
    -int depth_0() {}
    +int correct_depth_0() {}
    --- example/src/depth-1/depth-1.c
    +++ example/src/depth-1/depth-1.c
    @@ -1,1 +1,1 @@
    -int depth_1() {}
    +int correct_depth_1() {}
    --- example/src/depth-1/depth-2/depth-2.c
    +++ example/src/depth-1/depth-2/depth-2.c
    @@ -1,1 +1,1 @@
    -int depth_2() {}
    +int correct_depth_2() {}

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}]

let%expect_test "matcher_override" =
  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'(' '_unbalanced_match_' main.c -sequential -d %s -matcher .txt -diff" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/ignore-me/main.c
    +++ example/src/ignore-me/main.c
    @@ -1,1 +1,1 @@
    -int main() {}
    +int main_unbalanced_match_) {}
    --- example/src/main.c
    +++ example/src/main.c
    @@ -1,1 +1,1 @@
    -int main() {}
    +int main_unbalanced_match_) {} |}];

  let source = "hello world" in
  let src_dir = "example" ^/ "src" in
  let command_args = Format.sprintf "'(' '_unbalanced_match_' main.c -sequential -d %s -diff" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{| |}]

let%expect_test "infer_and_honor_extensions" =
  let source = "doesn't matter" in
  let src_dir = "example" ^/ "src" ^/ "honor-file-extensions" in
  let command_args = Format.sprintf "'foo()' 'bar()' .go -sequential -d %s -diff" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/honor-file-extensions/honor.pb.go
    +++ example/src/honor-file-extensions/honor.pb.go
    @@ -1,4 +1,4 @@
     func main() {
     // in a comment foo()
    -foo()
    +bar()
     } |}];

  let source = "doesn't matter" in
  let src_dir = "example" ^/ "src" ^/ "honor-file-extensions" in
  let command_args = Format.sprintf "'foo()' 'bar()' .generic -sequential -d %s -diff" src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- example/src/honor-file-extensions/honor.pb.generic
    +++ example/src/honor-file-extensions/honor.pb.generic
    @@ -1,3 +1,3 @@
     func main() {
    -// foo()
    +// bar()
     }

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}]

let%expect_test "diff_only" =
  let source = "hello world" in
  let command_args = Format.sprintf "'hello' 'world' -stdin -sequential -json-lines -json-only-diff" in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    {"uri":null,"diff":"--- /dev/null\n+++ /dev/null\n@@ -1,1 +1,1 @@\n-hello world\n\\ No newline at end of file\n+world world\n\\ No newline at end of file"}

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}];

  let source = "hello world" in
  let command_args = Format.sprintf "'hello' 'world' -stdin -sequential -json-only-diff" in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    -json-only-diff can only be supplied with -json-lines. |}]

let%expect_test "zip_exclude_dir_with_extension" =
  let source = "doesn't matter" in
  let zip = "example" ^/ "zip-test" ^/ "sample-repo.zip" in
  let exclude_dir = "sample-repo/vendor" in
  let command_args = Format.sprintf "'main' 'pain' .go -zip %s -sequential -diff -exclude-dir %s" zip exclude_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- sample-repo/src/main.go
    +++ sample-repo/src/main.go
    @@ -1,2 +1,2 @@
     // src
    -func main() {}
    +func pain() {} |}]

let%expect_test "zip_exclude_dir_no_extension" =
  let source = "doesn't matter" in
  let zip = "example" ^/ "zip-test" ^/ "sample-repo.zip" in
  let exclude_dir = "sample-repo/vendor" in
  let command_args = Format.sprintf "'main' 'pain' -zip %s -sequential -diff -exclude-dir %s" zip exclude_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    --- sample-repo/src/main.go
    +++ sample-repo/src/main.go
    @@ -1,2 +1,2 @@
     // src
    -func main() {}
    +func pain() {}

    WARNING: the GENERIC matcher was used, because a language could not be inferred from the file extension(s). The GENERIC matcher may miss matches. See '-list' to set a matcher for a specific language and to remove this warning. |}]

let%expect_test "invalid_path_with_error_message" =
  let source = "doesn't matter" in
  let command_args = Format.sprintf "'a' 'b' ./invalid/path" in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    No such file or directory: ./invalid/path. Comby interprets patterns containing '/' as file paths. If a pattern does not contain '/' (like '.ml'), it is considered a pattern where file endings must match the pattern. Please supply only valid file paths or patterns. |}]

let%expect_test "newline_separated_output"=
  let source = "a b c" in
  let match_template = ":[[1]]" in
  let rewrite_template = ":[[1]]" in
  let command_args =
    Format.sprintf "-stdin -sequential -stdout '%s' '%s' -n -matcher .generic"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|a
b
c
|}]

let%expect_test "warn_on_stdin_and_in_place_flags" =
  let source = "a b c" in
  let match_template = ":[[1]]" in
  let rewrite_template = ":[[1]]" in
  let command_args =
    Format.sprintf "-stdin -in-place '%s' '%s' -matcher .generic"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|WARNING: -in-place has no effect when -stdin is used. Ignoring -in-place.
|}]

let%expect_test "print_single_line_matches" =
  let source = {|
    let () = x in
    let () = y in
  |}
  in
  let match_template = "let ()" in
  let rewrite_template = "dont care" in
  let command_args =
    Format.sprintf "-stdin '%s' '%s' -match-only -matcher .generic"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|2:let ()
3:let ()

|}]

let%expect_test "print_multi_line_matches" =
  let source = {|
    let () = x in
    let

    ()
in
    let () = y in
  |}
  in
  let match_template = "let ()" in
  let rewrite_template = "dont care" in
  let command_args =
    Format.sprintf "-stdin '%s' '%s' -match-only -matcher .generic"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|2:let ()
3:let\n\n    ()
7:let ()

|}];

  let command_args =
    Format.sprintf "-stdin '%s' '%s' -match-only -count -matcher .generic"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|3 matches

|}];

  let command_args =
    Format.sprintf "-stdin '%s' '%s' -match-only -count -json-lines -matcher .generic"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":5,"line":2,"column":5},"end":{"offset":11,"line":2,"column":11}},"environment":[],"matched":"let ()"},{"range":{"start":{"offset":23,"line":3,"column":5},"end":{"offset":34,"line":5,"column":7}},"environment":[],"matched":"let\n\n    ()"},{"range":{"start":{"offset":42,"line":7,"column":5},"end":{"offset":48,"line":7,"column":11}},"environment":[],"matched":"let ()"}]}
WARNING: -count and -json-lines is specified. Ignoring -count.
|}];

  let command_args =
    Format.sprintf "-stdin '%s' '%s' -count -matcher .generic"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|3 matches

WARNING: -count only works with -match-only. Performing -match-only -count.
|}]

let%expect_test "unrecognized_matcher" =
  let source = {|dont care|} in
  let match_template = "dont care" in
  let rewrite_template = "dont care" in
  let command_args =
    Format.sprintf "-stdin '%s' '%s' -matcher invalid"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|The matcher "invalid" is not supported. See -list for supported matchers
|}]

let%expect_test "generic_matcher_ok" =
  let source = {|dont care|} in
  let match_template = "dont care" in
  let rewrite_template = "blah" in
  let command_args =
    Format.sprintf "-stdin '%s' '%s' -matcher .generic"
      match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|[0;31m------ [0m[0;1m/dev/null[0m
[0;32m++++++ [0m[0;1m/dev/null[0m
[0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
[0;41;30m-|[0m[0m[0;31mdont care[0m[0m
[0;42;30m+|[0m[0m[0;32mblah[0m[0m
|}]

let%expect_test "warn_on_anonymous_and_templates_flag" =
  let source = "(fun i -> j) (fun x -> x)" in
  let command_args =
    Format.sprintf "-stdin -sequential 'ignore' 'ignore' -templates example/templates/implicit-equals -matcher .ml -stdout"
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|(fun i -> j) identWARNING: Templates specified on the command line AND using -templates. Ignoring match
      and rewrite templates on the command line and only using those in directories.
|}]

let%expect_test "dump_stats" =
  let source = {|dont care|} in
  let match_template = "care" in
  let rewrite_template = "realy care" in
  let command_args = Format.sprintf "-stdin '%s' '%s' -stats -matcher .txt" match_template rewrite_template in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let stats_json = read_expect_stderr command source in
  (match Statistics.of_yojson (Yojson.Safe.from_string stats_json) with
   | Ok { number_of_files; lines_of_code; number_of_matches; _ } ->
     Format.printf "number_of_files: %d, lines_of_code: %d, number_of_matches: %d"
       number_of_files lines_of_code number_of_matches
   | Error _ -> print_string "Unexpected error");
  [%expect_exact {|number_of_files: 1, lines_of_code: 1, number_of_matches: 1|}]

let%expect_test "substitute_bad_parse" =
  let source = "dont care" in
  let match_template = "dont care" in
  let rewrite_template = "dont care" in
  let command_args = Format.sprintf "%s %s -substitute 'json'" match_template rewrite_template in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|Error, could not parse JSON to environment: Line 1, bytes 0-4:
Invalid token 'json'
|}]

let%expect_test "substitute_ok" =
  let source = "a match1 c d a match2 c d" in
  let match_template = "ignored" in
  let rewrite_template = ":[1] :[2]" in
  let environment = {|[{"variable":"1","value":"hole_1"},{"variable":"2","value":"hole_2"}]|} in
  let command_args =
    Format.sprintf "'%s' '%s' -stdin -match-only -matcher .txt -substitute '%s'" match_template rewrite_template environment
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  read_expect_stdin_and_stdout command source
  |> print_string;
  [%expect_exact {|hole_1 hole_2
|}]

let%expect_test "diff_patches_with_trailing_newlines" =
  let source = "dont care" in

  let src_dir = "example" ^/ "diff-no-newlines" ^/ "1" in
  let match_template = "1" in
  let rewrite_template = "2" in

  let command_args = Format.sprintf "'%s' '%s' -diff -f a -d %s -matcher .txt " match_template rewrite_template src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|--- example/diff-no-newlines/1/a
+++ example/diff-no-newlines/1/a
@@ -1,1 +1,1 @@
-1
\ No newline at end of file
+2
\ No newline at end of file
|}];

  let src_dir = "example" ^/ "diff-no-newlines" ^/ "2" in
  let command_args = Format.sprintf "'%s' '%s' -diff -f a -d %s -matcher .txt " match_template rewrite_template src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|--- example/diff-no-newlines/2/a
+++ example/diff-no-newlines/2/a
@@ -1,2 +1,2 @@
-1
-1
\ No newline at end of file
+2
+2
\ No newline at end of file
|}];

  let src_dir = "example" ^/ "diff-no-newlines" ^/ "3" in
  let command_args = Format.sprintf "'%s' '%s' -diff -f a -d %s -matcher .txt " match_template rewrite_template src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|--- example/diff-no-newlines/3/a
+++ example/diff-no-newlines/3/a
@@ -1,2 +1,2 @@
-1
+2
 2
\ No newline at end of file
|}];

  (* Introduce newline *)
  let match_template = "1" in
  let rewrite_template = "2\n" in
  let src_dir = "example" ^/ "diff-no-newlines" ^/ "4" in
  let command_args = Format.sprintf "'%s' '%s' -diff -f a -d %s -matcher .txt " match_template rewrite_template src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|--- example/diff-no-newlines/4/a
+++ example/diff-no-newlines/4/a
@@ -1,1 +1,1 @@
-1
\ No newline at end of file
+2
|}];

  (* Delete newline *)
  let match_template = "1\n" in
  let rewrite_template = "2" in
  let src_dir = "example" ^/ "diff-no-newlines" ^/ "5" in
  let command_args = Format.sprintf "'%s' '%s' -diff -f a -d %s -matcher .txt " match_template rewrite_template src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|--- example/diff-no-newlines/5/a
+++ example/diff-no-newlines/5/a
@@ -1,1 +1,1 @@
-1
+2
\ No newline at end of file
|}]

let%expect_test "diff_patches_preserve_slash_r" =
  let source = "dont care" in

  let src_dir = "example" ^/ "diff-preserve-slash-r" in
  let match_template = "1" in
  let rewrite_template = "2" in

  let command_args = Format.sprintf "'%s' '%s' -json-lines -json-only-diff -f a -d %s -matcher .txt " match_template rewrite_template src_dir in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect_exact {|{"uri":"example/diff-preserve-slash-r/a","diff":"--- example/diff-preserve-slash-r/a\n+++ example/diff-preserve-slash-r/a\n@@ -1,3 +1,3 @@\n-1\r\n+2\r\n 2\r\n 3\r"}
|}]

let%expect_test "warn_on_match_template_starts_with_everything_hole" =
  let source = "hello world\n" in
  let match_template = ":[2] :[[1]]" in
  let rewrite_template = ":[1]" in
  let command_args =
    Format.sprintf "-stdin -sequential '%s' '%s' -stdout -f .c " match_template rewrite_template
  in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_expect_stdin_and_stdout command source in
  print_string result;
  [%expect{|
    world
    WARNING: The match template starts with a :[hole]. You almost never want to start a template with :[hole], since it matches everything including newlines up to the part that comes after it. This can make things slow. :[[hole]] might be what you're looking for instead, like when you want to match an assignment foo = bar(args) on a line, use :[[var]] = bar(args). :[hole] is typically useful inside balanced delimiters. |}]
