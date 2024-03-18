# comby

[![Apache-2.0](https://img.shields.io/badge/license-Apache-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.com/comby-tools/comby.svg?branch=master)](https://travis-ci.com/comby-tools/comby)
[![codecov](https://codecov.io/gh/comby-tools/comby/branch/master/graph/badge.svg?token=I1F0PC32E5)](https://codecov.io/gh/comby-tools/comby)
[![Downloads](https://img.shields.io/github/downloads/comby-tools/comby/total.svg?color=orange)](Downloads)
[![Commit](https://img.shields.io/github/last-commit/comby-tools/comby.svg)](Commit)
[![Gitter](https://img.shields.io/gitter/room/comby-tools/comby.svg?color=teal)](https://gitter.im/comby-tools/community)

![](https://user-images.githubusercontent.com/888624/64916761-0b657780-d752-11e9-96e2-cd81a2681139.gif)

### See the [usage documentation](https://comby.dev).
[A short example below](https://github.com/comby-tools/comby#isnt-a-regex-approach-like-sed-good-enough) shows how comby simplifies matching and rewriting compared to regex approaches like `sed`.

<details>
  <summary>Comby supports interactive review mode (click here to see it in action).</summary>

![](https://user-images.githubusercontent.com/888624/69503010-b8870980-0ed2-11ea-828d-68c152ed9def.gif)

</details>

**Need help writing patterns or have other problems? Post them in [Gitter](https://gitter.im/comby-tools/community).**

## Install (pre-built binaries)

### Mac OS X

- `brew install comby`

### Ubuntu Linux

- `bash <(curl -sL get-comby.netlify.app)`

- **Other Linux distributions**: The PCRE library is dynamically linked in the Ubuntu binary. For other distributions like Arch Linux, a fixup is needed: `sudo ln -s /usr/lib/libpcre.so /usr/lib/libpcre.so.3`. On Fedora, use `sudo ln -s /usr/lib64/libpcre.so /usr/lib64/libpcre.so.3`. Alternatively, consider [building from source](https://github.com/comby-tools/comby#build-from-source).


### Windows

- [Install the Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and install Ubuntu. Then run `bash <(curl -sL get.comby.dev)`


### Docker

- `docker pull comby/comby`

<details>
  <summary>click to expand an example invocation for the docker image</summary>

Running with docker on `stdin`:

```bash
docker run -a stdin -a stdout -a stderr -i comby/comby '(:[emoji] hi)' 'bye :[emoji]' lisp -stdin <<< '(👋 hi)'
```

<img width="500" src="https://user-images.githubusercontent.com/888624/64924862-0edf1a00-d7b7-11e9-9c2e-cfeafde5bb4b.png">

</details>



### Or [try it live](https://bit.ly/2UXkonD).

## Isn't a regex approach like sed good enough?

Sometimes, yes. But often, small changes and refactorings are complicated by nested expressions, comments, or strings. Consider the following C-like snippet. Say the challenge is to rewrite the two `if` conditions to the value `1`. Can you write a regular expression that matches the contents of the two if condition expressions, and only those two? Feel free to share your pattern with [@rvtond](https://twitter.com/rvtond) on Twitter.

```c
if (fgets(line, 128, file_pointer) == Null) // 1) if (...) returns 0
      return 0;
...
if (scanf("%d) %d", &x, &y) == 2) // 2) if (scanf("%d) %d", &x, &y) == 2) returns 0
      return 0;
```

To match these with comby, all you need to write is `if (:[condition])`, and specify one flag that this language is C-like. The replacement is `if (1)`. See the [live example](https://bit.ly/30935ou).

## Build from source

- Install [opam](https://opam.ocaml.org/doc/Install.html). TL;DR do `sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)`

- Run this if you don't have OCaml installed (it bootstraps the OCaml compiler):

```
opam init
opam switch create 4.12.0 4.12.0
```

- Run `eval $(opam env)`


- Install OS dependencies:

  - **Linux:** `sudo apt-get install autoconf libpcre3-dev pkg-config zlib1g-dev m4 libgmp-dev libev4 libsqlite3-dev`

  - **Mac:** `brew install pkg-config gmp pcre libev`

- Then install the library dependencies:

```
git clone https://github.com/comby-tools/comby
cd comby 
opam install . --deps-only
```

- Build and test

```
make
make test
```

- Install `comby` on your `PATH` by running

```
make install
```
