Create files with deterministic modification times.

  $ cat > prev <<EOF
  > hello world
  > EOF

  $ touch prev -d '1970-01-01T00:00:00Z'

  $ cat > next <<EOF
  > hello
  > EOF

  $ touch next -d '1970-01-02T00:00:00Z'

Modification times are displayed in UTC.

  $ TZ=America/New_York patdiff -default prev next -html
  <pre style="font-family:consolas,monospace">
  <span style="color:#880000">------ </span><span style="font-weight:bold">prev 1970-01-01 00:00:00.000000Z</span>
  <span style="color:#008800">++++++ </span><span style="font-weight:bold">next 1970-01-02 00:00:00.000000Z</span>
  <span style="color:#000000"><span style="background-color:#c0c0c0">@|</span></span><span style="font-weight:bold">-1,1 +1,1</span> ============================================================
  <span style="color:#000000"><span style="background-color:#888800">!|</span></span>hello<span style="color:#880000"> world</span>
  </pre>
  [1]

HTML output includes mtime when [-alt-{prev,next}] is passed.

  $ TZ=America/New_York patdiff -default prev next -html -alt-prev a -alt-next b
  <pre style="font-family:consolas,monospace">
  <span style="color:#880000">------ </span><span style="font-weight:bold">a 1970-01-01 00:00:00.000000Z</span>
  <span style="color:#008800">++++++ </span><span style="font-weight:bold">b 1970-01-02 00:00:00.000000Z</span>
  <span style="color:#000000"><span style="background-color:#c0c0c0">@|</span></span><span style="font-weight:bold">-1,1 +1,1</span> ============================================================
  <span style="color:#000000"><span style="background-color:#888800">!|</span></span>hello<span style="color:#880000"> world</span>
  </pre>
  [1]
