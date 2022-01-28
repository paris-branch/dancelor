.PHONY: build doc release test local serve init-only check-tunes clean

build:
	cd share/static/style && sass style.scss ../style.css
	dune build @install
	ln -sf _build/install/default/bin .
	ln -sf ../../_build/install/default/share/dancelor share/static/

doc:
	dune build @doc
	ln -sf _build/default/_doc/_html doc

release:
	dune build --profile=release @install
	ln -sf _build/install/default/bin .
	ln -sf ../../_build/install/default/share/dancelor share/static/

test:
	dune runtest

local: build
	bin/dancelor-server --config share/config.json --no-routines --no-sync-storage --no-write-storage

serve: release
	bin/dancelor-server --config share/config.json

init-only: release
	bin/dancelor-server --config share/config.json --init-only

check-tunes: build
	bin/dancelor-server --config share/config.json --heavy-routines --no-sync-storage --no-write-storage --loglevel info

clean:
	dune clean
	rm -f *.opam
	rm -f bin doc
	rm -f share/static/dancelor
