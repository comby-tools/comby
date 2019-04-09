all: build comby

build:
	@dune build

comby comby-server:
	@ln -s _build/install/default/bin/$@ ./$@

install:
	@dune install

doc:
	@dune build @doc

test:
	@dune runtest

clean:
	@dune clean

uninstall:
	@dune uninstall

promote:
	@dune promote

.PHONY: all build install test clean promote
