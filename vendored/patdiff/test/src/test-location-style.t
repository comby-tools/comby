Test omake-style locations needed for emacs to jump to test failures.
 
  $ cat - > old <<EOF
  > mary had a little lamb
  > its fleece was white as snow
  > hello world
  > bar
  > baz
  > EOF
 
  $ cat - > new <<EOF
  > mary had a little lamb
  > its fleece was white as snow
  > hello
  > bar
  > EOF
 
  $ patdiff -location-style omake old new | visible_colors
  (fg:red)------ (+bold)old
  (fg:green)++++++ (+bold)new
  File "old", line 3, characters 0-1:
  (fg:black) |(off)mary had a little lamb
  (fg:black) |(off)its fleece was white as snow
  (fg:black bg:yellow)!|(off)hello(fg:red) world
  (fg:black) |(off)bar
  (fg:black bg:red)-|(fg:red)baz

Test omitting line numbers

  $ patdiff old new | visible_colors
  (fg:red)------ (+bold)old
  (fg:green)++++++ (+bold)new
  (fg:black)@|(+bold)-1,5 +1,4(off) ============================================================
  (fg:black) |(off)mary had a little lamb
  (fg:black) |(off)its fleece was white as snow
  (fg:black bg:yellow)!|(off)hello(fg:red) world
  (fg:black) |(off)bar
  (fg:black bg:red)-|(fg:red)baz

  $ patdiff -location-style none old new | visible_colors
  (fg:red)------ (+bold)old
  (fg:green)++++++ (+bold)new
  (fg:black)@|(off) ============================================================
  (fg:black) |(off)mary had a little lamb
  (fg:black) |(off)its fleece was white as snow
  (fg:black bg:yellow)!|(off)hello(fg:red) world
  (fg:black) |(off)bar
  (fg:black bg:red)-|(fg:red)baz
