Test that we ignore or consider whitespace as directed.

  $ cat - > prev <<EOF
  > this is a file with whitespace variously applied hg across the lines in an arbitrary manner
  > EOF

  $ cat - > next <<EOF
  > 
  >  this is  a file 	with  	  whitespace
  >  variously  applied
  > hg
  > across the
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > lines in an arbitrary
  > manner
  > EOF

Expect that we ignore whitespace changes.

  $ patdiff -default prev next

Expect no spurious diffs.

  $ patdiff -default next next -keep-whitespace

Expect whitespace changes should be detected.

  $ patdiff -default prev next -keep-whitespace | visible_colors
  (fg:red)------ (+bold)prev
  (fg:green)++++++ (+bold)next
  (fg:black)@|(+bold)-1,1 +1,17(off) ============================================================
  (fg:black bg:red)-|(off)this is(fg:red +reverse) (off)a file(fg:red +reverse) (off)with(fg:red +reverse) (off)whitespace(fg:red +reverse) (off)variously(fg:red +reverse) (off)applied(fg:red +reverse) (off)hg(fg:red +reverse) (off)across the(fg:red +reverse) (off)lines in an arbitrary(fg:red +reverse) (off)manner
  (fg:black bg:green)+|
  (fg:black bg:green)+|(fg:green +reverse) (off)this is(fg:green +reverse)  (off)a file(fg:green +reverse) 	(off)with(fg:green +reverse)  	  (off)whitespace
  (fg:black bg:green)+|(fg:green +reverse) (off)variously(fg:green +reverse)  (off)applied
  (fg:black bg:green)+|(off)hg
  (fg:black bg:green)+|(off)across the
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|(off)lines in an arbitrary
  (fg:black bg:green)+|(off)manner

Note that some whitespace changes are still ignored, just not those involving substituting a newline for a space.

  $ cat - > prev <<EOF
  > this is a file with whitespace but no newlines
  > EOF

  $ cat - > next <<EOF
  >  this is  a file 	with  	  whitespace    but           no newlines  
  > EOF


  $ patdiff -default prev next

Create some python files with the .py extension
  $ cat - > prev.py <<EOF
  > print("hello")
  > EOF
  $ cat - > next.py <<EOF
  > if True:
  >   print("hello")
  > EOF
  $ patdiff prev.py next.py | visible_colors
  (fg:red)------ (+bold)prev.py
  (fg:green)++++++ (+bold)next.py
  (fg:black)@|(+bold)-1,1 +1,2(off) ============================================================
  (fg:black bg:yellow)!|(fg:green)if True:
  (fg:black bg:yellow)!|(fg:green +reverse)  (off)print("hello")

Create some python files that get detected with the shebang
  $ cat - > prev <<EOF
  > #!/usr/bin/python
  > print("hello")
  > EOF
  $ cat - > next <<EOF
  > #!/usr/bin/python
  > if True:
  >   print("hello")
  > EOF
  $ patdiff prev next | visible_colors
  (fg:red)------ (+bold)prev
  (fg:green)++++++ (+bold)next
  (fg:black)@|(+bold)-1,2 +1,3(off) ============================================================
  (fg:black) |(off)#!/usr/bin/python
  (fg:black bg:yellow)!|(fg:green)if True:
  (fg:black bg:yellow)!|(fg:green +reverse)  (off)print("hello")
