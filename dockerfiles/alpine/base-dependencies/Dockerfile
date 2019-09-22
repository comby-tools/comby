FROM ocaml/opam2:alpine-3.9-ocaml-4.07

WORKDIR /home/opam

RUN sudo apk --no-cache add \
    pcre \
    m4 \
    linux-headers \
    perl \
    gmp-dev \
    zlib-dev

RUN opam update 

RUN eval $(opam env) \
    && git clone https://github.com/comby-tools/comby.git \
    && cd comby \
    && git checkout set-up-opam-deps-only \
    && opam install . --deps-only -y \
    && cd ..
