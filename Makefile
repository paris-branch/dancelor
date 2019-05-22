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

local: build
	bin/dancelor-server --config share/config.json --no-routines --no-sync-storage

serve: release
	bin/dancelor-server --config share/config.json

init-only: release
	bin/dancelor-server --config share/config.json --init-only

clean:
	dune clean
	rm -f bin
	rm -f share/static/dancelor
