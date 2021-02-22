all: build comby comby-server benchmark

build:
	@rm comby comby-server benchmark
	@dune build --profile dev
	@ln -sfn _build/install/default/bin/comby comby
	@ln -sfn _build/install/default/bin/comby-server comby-server
	@ln -sfn _build/install/default/bin/benchmark benchmark

build-with-coverage:
	@rm comby comby-server benchmark
	@dune build --instrument-with bisect_ppx
	@ln -sfn _build/install/default/bin/comby comby
	@ln -sfn _build/install/default/bin/comby-server comby-server
	@ln -sfn _build/install/default/bin/benchmark benchmark

release:
	@rm comby comby-server benchmark
	@dune build --profile release
	@ln -sfn _build/install/default/bin/comby comby
	@ln -sfn _build/install/default/bin/comby-server comby-server
	@ln -sfn _build/install/default/bin/benchmark benchmark

byte:
	@dune build src/main.bc

# Uncomment this when dune is fixed: https://github.com/ocaml/dune/issues/4258
# comby comby-server benchmark:
#	@ln -s _build/install/default/bin/$@ ./$@

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

.PHONY: all build build-with-coverage release run-server run-staging-server install doc test coverage clean uninstall promote docker-test-build comby comby-server benchmark
