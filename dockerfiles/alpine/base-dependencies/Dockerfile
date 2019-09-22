FROM ocaml/opam2:alpine-3.10-ocaml-4.07

WORKDIR /home/opam

RUN sudo apk --no-cache add \
    pcre-dev \
    m4 \
    linux-headers \
    perl \
    gmp-dev \
    zlib-dev

RUN eval $(opam env) \
    && git clone https://github.com/comby-tools/comby.git \
    && cd comby \
    && opam install . --deps-only -y \
    && cd ..
    && rm -rf comby
