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

# With rule options in src/dune. Disable inlining to get a good size but always working JS.
js:
	@dune build @js
	@cp ./_build/default/js/comby.js comby.js
	@chmod 0644 comby.js
	@sed -i .orig "s|steps=\[0,20|steps=\[0,1|" comby.js
	@sed -i .orig "s|max_steps=20|max_steps=1|" comby.js
	@rm -rf comby.js.orig
	@du -h comby.js

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

.PHONY: all build build-with-coverage release install doc test coverage clean uninstall promote docker-test-build js
