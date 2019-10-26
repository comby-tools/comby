FROM ocaml/opam2:ubuntu-lts

WORKDIR /home/opam

RUN sudo apt-get install -y \
    libpcre3-dev \
    pkg-config \
    zlib1g-dev \
    m4 \
    libgmp-dev

RUN eval $(opam env) 

WORKDIR /tmp/comby

COPY Makefile /tmp/comby/
COPY comby.opam /tmp/comby/
COPY dune /tmp/comby/
COPY src /tmp/comby/src
COPY lib /tmp/comby/lib
COPY test /tmp/comby/test

RUN sudo chown -R $(whoami) .

RUN opam install . --deps-only -y
RUN make release && make test
