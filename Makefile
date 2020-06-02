all: build comby comby-server benchmark

build:
	dune build --profile dev

build-with-coverage:
	@BISECT_ENABLE=yes dune build --profile dev

release:
	@dune build --profile release

byte:
	@dune build src/main.bc

comby comby-server benchmark:
	@ln -s _build/install/default/bin/$@ ./$@

run-server:
	@./comby-server -p 8888

run-staging-server:
	@./comby-server -p 8887

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

docker-test-build:
	docker build -t comby-local-test-build .

.PHONY: all build build-with-coverage release run-server run-staging-server install doc test coverage clean uninstall promote docker-test-build
