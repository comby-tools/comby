all: build

build:
	@dune build --profile dev
	@ln -sfn _build/install/default/bin/comby comby

build-with-coverage:
	@dune build --instrument-with bisect_ppx --force
	@ln -sfn _build/install/default/bin/comby comby

release:
	@dune build --profile release
	@ln -sfn _build/install/default/bin/comby comby

byte:
	@dune build src/main.bc

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
	@rm -rf comby

uninstall:
	@dune uninstall

promote:
	@dune promote

docker-test-build:
	docker build -t comby-local-test-build .

.PHONY: all build build-with-coverage release install doc test coverage clean uninstall promote docker-test-build
