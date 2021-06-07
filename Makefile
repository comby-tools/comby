all: build

build:
	@rm -rf comby 
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

# Uncomment this when dune is fixed: https://github.com/ocaml/dune/issues/4258
# comby:
#	@ln -s _build/install/default/bin/$@ ./$@

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

.PHONY: all build build-with-coverage release install doc test coverage clean uninstall promote docker-test-build comby 
