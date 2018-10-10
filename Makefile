.PHONY: build clean

build:
	dune build @install
	ln -sf _build/install/default/bin .

test:
	dune runtest

serve: build
	bin/dancelor-server

clean:
	dune clean
	rm -f bin
