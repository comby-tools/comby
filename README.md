# comby

## Build

- Install [opam](https://opam.ocaml.org/doc/Install.html)

- Create a new switch if you don't have OCaml installed

```
opam init
opam switch create 4.05.0 4.05.0 
```

- Install dependencies

### Linux

```
sudo apt-get install pkg-config pcre
```

### Mac

```
brew install pkg-config pcre
```

- Install opam libraries

```
opam install ppx_deriving_yojson
opam install core
opam install ppxlib
opam install ppx_deriving
opam install angstrom
opam install hack_parallel
opam install opium
opam install pcre
opam install oasis
```

- Install mparser

```
git clone https://github.com/comby-tools/mparser
oasis setup
ocaml setup.ml -configure --enable-pcre --enable-re
ocaml setup.ml -build
ocaml setup.ml -install
```

- Build and test

```
make
make test
```

## Running

`./comby MATCH_TEMPLATE REWRITE_TEMPLATE` by default rewrites all files
in-place in the current directory and all subdirectories. 

- Adding `-f .ml` will rewrite all files ending in `.ml`. Adding `-f README.md` will rewrite all files
ending in `README.md`

- Input can also be fed via `-stdin` instead of rewriting files:

```bash
./comby -stdin 'printf(":[1] :[2]")' 'printf("comby, :[1]")' << EOF 2> /dev/null
int main(void) {
  printf("hello world");
}
EOF
```

Outputs:

```
int main(void) {
  printf("comby, hello");
}
```
- Adding a `-json` flag will output JSON content of the rewrite:

```json
[
  {
    "range": {
      "start": { "offset": 19, "line": -1, "column": -1 },
      "end": { "offset": 41, "line": -1, "column": -1 }
    },
    "replacement_content": "printf(\"comby, hello\")",
    "environment": [
      [
        "1",
        {
          "value": "hello",
          "range": {
            "start": { "offset": 15, "line": -1, "column": -1 },
            "end": { "offset": 20, "line": -1, "column": -1 }
          }
        }
      ]
    ]
  }
]
```

Note: line and column offsets are not currently calculated for rewrite ranges.

- Adding a `-match-only` flag will output JSON content of all matches:

```json
[
  {
    "range": {
      "start": { "offset": 19, "line": 1, "column": 20 },
      "end": { "offset": 40, "line": 1, "column": 41 }
    },
    "environment": [
      [
        "1",
        {
          "value": "hello",
          "range": {
            "start": { "offset": 27, "line": 1, "column": 28 },
            "end": { "offset": 32, "line": 1, "column": 33 }
          }
        }
      ],
      [
        "2",
        {
          "value": "world",
          "range": {
            "start": { "offset": 33, "line": 1, "column": 34 },
            "end": { "offset": 38, "line": 1, "column": 39 }
          }
        }
      ]
    ],
    "matched": "printf(\"hello world\")"
  }
]
```

See other flags for more options with `./comby -h`:

```
Run a rewrite pass.

  comby [MATCH_TEMPLATE REWRITE_TEMPLATE]

=== flags ===

  [-directory path]     Run on files in a directory. Default is current
                        directory: /Users/rvt/comby
  [-filter extensions]  CSV of extensions to include
  [-jobs n]             Number of worker processes
  [-json]               Output JSON format for matches or rewrite text to stdout
  [-match-only]         Only perform matching (ignore rewrite templates)
  [-rule rule]          Apply rules to matches. Respects -f
  [-sequential]         Run sequentially
  [-stdin]              Read source from stdin
  [-templates path]     CSV of directories containing templates
  [-timeout seconds]    Set match timeout on a source. Default: 3
  [-verbose]            Log to /tmp/comby.out
  [-build-info]         print info about this build and exit
  [-version]            print the version of this build and exit
  [-help]               print this help text and exit
                        (alias: -?)
```
