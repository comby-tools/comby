Create files with spaces in their name.

  $ cat > 'prev file' <<EOF
  > hello
  > world
  > EOF

  $ cp 'prev file' 'next file'

Normal patdiff works fine.

  $ patdiff 'prev file' 'next file'

Patdiff with [-double-check] properly quotes the arguments to [cmp].

  $ patdiff -double-check 'prev file' 'next file'
