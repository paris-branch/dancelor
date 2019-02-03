.PHONY: build clean

build:
	dune build @install
	ln -sf _build/install/default/bin .
	ln -sf ../../_build/install/default/share/dancelor share/static/

release:
	dune build --profile=release @install
	ln -sf _build/install/default/bin .
	ln -sf ../../_build/install/default/share/dancelor share/static/

test:
	dune runtest

serve: release
	bin/dancelor-server share/config.json

clean:
	dune clean
	rm -f bin
	rm -f share/static/dancelor
