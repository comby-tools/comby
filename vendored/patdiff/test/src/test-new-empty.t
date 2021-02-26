Test diffing against empty files.

  $ cat - > prev <<EOF
  > oneline
  > EOF

  $ next=$(mktemp)
  $ cat - > next <<EOF
  > EOF

Expect all red.

  $ patdiff -default prev next | visible_colors
  (fg:red)------ (+bold)prev
  (fg:green)++++++ (+bold)next
  (fg:black)@|(+bold)-1,1 +1,0(off) ============================================================
  (fg:black bg:red)-|(fg:red)oneline

Expect all green.

  $ patdiff -default next prev | visible_colors
  (fg:red)------ (+bold)next
  (fg:green)++++++ (+bold)prev
  (fg:black)@|(+bold)-1,0 +1,1(off) ============================================================
  (fg:black bg:green)+|(fg:green)oneline
