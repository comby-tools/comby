all: build comby comby-server

build:
	@dune build

comby comby-server:
	@ln -s _build/install/default/bin/$@ ./$@

run-server:
	@./comby-server -p 7777

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
