all: build comby comby-server

build:
	@BISECT_ENABLE=yes dune build --profile dev

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

coverage:
	@bisect-ppx-report -I _build/default/ -html coverage/ `find . -name 'bisect*.out'`

clean:
	@dune clean

uninstall:
	@dune uninstall

promote:
	@dune promote

.PHONY: all build install test clean promote
