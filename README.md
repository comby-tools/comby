# comby

[![Apache-2.0](https://img.shields.io/badge/license-Apache-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.com/comby-tools/comby.svg?branch=master)](https://travis-ci.com/comby-tools/comby)
![Coveralls github](https://img.shields.io/coveralls/github/comby-tools/comby)
[![Downloads](https://img.shields.io/github/downloads/comby-tools/comby/total.svg?color=orange)](Downloads)
[![Commit](https://img.shields.io/github/last-commit/comby-tools/comby.svg)](Commit)
[![Gitter](https://img.shields.io/gitter/room/comby-tools/comby.svg?color=teal)](https://gitter.im/comby-tools/community)

![](https://user-images.githubusercontent.com/888624/64916761-0b657780-d752-11e9-96e2-cd81a2681139.gif)

### See the [usage documentation](https://comby.dev).
[A short example below](https://github.com/comby-tools/comby#arent-regex-approaches-like-sed-good-enough) shows how comby simplifies matching and rewriting compared to regex approaches like `sed`.

**Need help writing patterns or have other problems? Consider posting in [Gitter](https://gitter.im/comby-tools/community).**


## Install (pre-built binaries)

### Mac OS X

- `bash <(curl -sL get.comby.dev)`

- If you run into `dyld: Library not loaded: /usr/local/opt/pcre/lib/libpcre.1.dylib`, try run this fixup:

<details>
  <summary>click to expand</summary>
  
```
install_name_tool -change /usr/local/opt/pcre/lib/libpcre.1.dylib /usr/local/brew/lib/libpcre.1.dylib /usr/local/bin/comby`
```

</details>

### Ubuntu Linux

- `bash <(curl -sL get.comby.dev)`

- **Arch and other Linux**: The PCRE library is dynamically linked in the Ubuntu binary. For other distributions, like Arch, a fixup is needed: `ln -s /usr/lib/libpcre.so /usr/lib/libpcre.so.3`. Alternatively, consider [building from source](https://github.com/comby-tools/comby#build-from-source).


### Windows

- [Install the Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10) and install Ubuntu. Then run `bash <(curl -sL get.comby.dev)`


### Docker

- `docker pull comby/comby`

<details>
  <summary>click to expand an example invocation for the docker image</summary>
  
Running with docker on `stdin`:

```bash
echo '(👋 hi)' | docker run -a stdin -a stdout -i comby/comby '(:[emoji] hi)' 'bye :[emoji]' lisp -stdin
```

<img width="500" src="https://user-images.githubusercontent.com/888624/64924862-0edf1a00-d7b7-11e9-9c2e-cfeafde5bb4b.png">

</details>



### Or [try it live](https://bit.ly/2UXkonD).

## Aren't regex approaches like sed good enough?

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

- Install [opam](https://opam.ocaml.org/doc/Install.html)

- Create a new switch if you don't have OCaml installed:

```
opam init
opam switch create 4.08.1 4.08.1
```

- [Install OS dependencies:](#os-dependencies)

  - **Linux:** `sudo apt-get install pkg-config libpcre3-dev`

  - **Mac:** `brew install pkg-config pcre`

- Then install the library dependencies:

```
git clone https://github.com/comby-tools/comby
cd comby && opam install . --deps-only -y
```

- Build and test

```
make
make test
```

- If you want to install `comby` on your `PATH`, run

```
make install
```
