FROM ocaml/opam2:alpine-3.9-ocaml-4.07

WORKDIR /home/opam

RUN sudo apk --no-cache add \
    pcre-dev \
    m4 \
    linux-headers \
    perl \
    gmp-dev \
    zlib-dev

RUN opam update && opam install -y \
    ppx_deriving_yojson \
    core \
    ppxlib \
    ppx_deriving \ 
    tls \
    angstrom \
    hack_parallel \
    opium \
    pcre \
    oasis \
    camlzip \
    bisect_ppx \
    patdiff

RUN eval $(opam env) \
    && git clone https://github.com/comby-tools/mparser.git \
    && cd mparser \
    && oasis setup \
    && ocaml setup.ml -configure --enable-pcre --enable-re \
    && ocaml setup.ml -build \
    && ocaml setup.ml -install \
    && cd ..

ARG CACHEBUST=1

RUN eval $(opam env) \
    && git clone https://github.com/comby-tools/comby.git \
    && make -C comby \
    && make -C comby test
