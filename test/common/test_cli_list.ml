open Test_cli_helper

let%expect_test "list_languages" =
  let command_args = "-list" in
  let command = Format.sprintf "%s %s" binary_path command_args in
  let result = read_output command in
  print_string result;
  [%expect_exact
    {|Option              Language  
 -matcher .s        Assembly  
 -matcher .sh       Bash      
 -matcher .c        C         
 -matcher .cs       C#        
 -matcher .css      CSS       
 -matcher .dart     Dart      
 -matcher .dyck     Dyck      
 -matcher .clj      Clojure   
 -matcher .v        Coq       
 -matcher .elm      Elm       
 -matcher .erl      Erlang    
 -matcher .ex       Elixir    
 -matcher .f        Fortran   
 -matcher .fsx      F#        
 -matcher .go       Go        
 -matcher .html     HTML      
 -matcher .hs       Haskell   
 -matcher .tf       Terraform (HashiCorp Configuration Language
 -matcher .java     Java      
 -matcher .js       JavaScript
 -matcher .jsx      JSX       
 -matcher .json     JSON      
 -matcher .jsonc    JSONC     
 -matcher .gql      GraphQL   
 -matcher .dhall    Dhall     
 -matcher .jl       Julia     
 -matcher .kt       Kotlin    
 -matcher .tex      LaTeX     
 -matcher .lisp     Lisp      
 -matcher .move     Move      
 -matcher .nim      Nim       
 -matcher .m        MATLAB    
 -matcher .ml       OCaml     
 -matcher .paren    Paren     
 -matcher .pas      Pascal    
 -matcher .php      PHP       
 -matcher .py       Python    
 -matcher .re       Reason    
 -matcher .r        R         
 -matcher .rb       Ruby      
 -matcher .rs       Rust      
 -matcher .scala    Scala     
 -matcher .sol      Solidity  
 -matcher .sql      SQL       
 -matcher .swift    Swift     
 -matcher .txt      Text      
 -matcher .ts       TypeScript
 -matcher .tsx      TSX       
 -matcher .xml      XML       
 -matcher .zig      Zig       
 -matcher .generic  Generic   
|}]
