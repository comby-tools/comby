all: build

build:
	@dune build

test:
#	@wget -O test/master.zip https://github.com/torvalds/linux/archive/master.zip
	@cd test && ./test.sh

clean:
	@rm -f *.so *.a *.cma *.cmo *.o *.cmi
	@dune clean

.PHONY: all build test clean
