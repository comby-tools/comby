FROM ocaml/opam2:ubuntu-18.04-opam

WORKDIR /home/comby

RUN sudo apt-get install -y \
	libpcre3-dev \
	pkg-config \
	zlib1g-dev \
	m4 \
	libgmp-dev

RUN opam init --no-setup --disable-sandboxing --compiler=4.09.0

COPY Makefile /home/comby/
COPY comby.opam /home/comby/
COPY dune /home/comby/
COPY docs /home/comby/docs
COPY src /home/comby/src
COPY lib /home/comby/lib
COPY test /home/comby/test
COPY push-coverage-report.sh /home/comby/

RUN sudo chown -R $(whoami) /home/comby

RUN eval $(opam env) && opam install . --deps-only -y
# Next command must be a single run command for $(opam env) to take effect.
RUN eval $(opam env) && make && make test && make clean && make release

# The binary is now available in /home/comby/_build/default/src/main.exe
