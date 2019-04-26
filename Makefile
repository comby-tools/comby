all: build comby comby-server

build:
	@dune build --profile dev

release:
	@dune build --profile release

comby comby-server:
	@ln -s _build/install/default/bin/$@ ./$@

run-server:
	@./comby-server -p 8888

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
