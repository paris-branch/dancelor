.PHONY: build clean

build:
	dune build @install
	ln -sf _build/install/default/bin .
	ln -sf ../_build/install/default/share/dancelor share/static/

test:
	dune runtest

serve: build
	bin/dancelor-server share/config.json

clean:
	dune clean
	rm -f bin
	rm -f share/dancelor
