.PHONY: build dev entr local dev-test clean

build:
	dune build @install
	ln -sf _build/install/default/bin .

dev: build
	bin/dancelor --config assets/config.local.json

entr:
	find src/ -type f | entr -ccrd make dev

local: build
	bin/dancelor --config assets/config.local.json --write-storage

dev-test: build
	bin/dancelor --config tests/config.json

clean:
	dune clean
	rm -f bin
