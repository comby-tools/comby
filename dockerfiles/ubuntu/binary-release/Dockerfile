FROM ocaml/opam2:ubuntu-18.04-opam

WORKDIR /home/comby

RUN sudo apt-get update && sudo apt-get install -y \
    autoconf \
    libpcre3-dev \
    pkg-config \
    zlib1g-dev \
    m4 \
    libgmp-dev \
    libev-dev \
    libsqlite3-dev

RUN opam init --no-setup --disable-sandboxing --compiler=4.11.0
RUN eval $(opam env) && opam repository set-url default https://opam.ocaml.org && opam update

COPY Makefile /home/comby/
COPY comby.opam /home/comby/
COPY comby-kernel.opam /home/comby/
COPY comby-semantic.opam /home/comby/
COPY dune /home/comby/
COPY dune-project /home/comby/dune-project
COPY docs /home/comby/docs
COPY src /home/comby/src
COPY lib /home/comby/lib
COPY test /home/comby/test
COPY push-coverage-report.sh /home/comby/

RUN sudo chown -R $(whoami) /home/comby
RUN eval $(opam env) && opam install . --deps-only -y
RUN opam exec -- make build
RUN opam exec -- dune runtest
RUN opam exec -- dune clean
# UGH. stop dune from complaining
RUN rm -rf comby 
RUN opam exec -- dune build --profile release

# The binary is now available in /home/comby/_build/default/src/main.exe
