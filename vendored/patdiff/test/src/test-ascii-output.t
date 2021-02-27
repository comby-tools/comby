Expect no ANSI escapes and no intra-line refinement.

  $ cat > prev <<EOF
  > hello world
  > EOF

  $ cat > next <<EOF
  > hello
  > EOF

  $ patdiff -default prev next -ascii
  ------ prev
  ++++++ next
  @|-1,1 +1,1 ============================================================
  -|hello world
  +|hello
  [1]
