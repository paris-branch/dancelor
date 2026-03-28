.PHONY: build dev entr local dev-test clean

build:
	dune build @install @runtest

entr:
	watchexec --clear --restart -- 'dune build @install @runtest && dune exec dancelor -- --config assets/config.local.json'

dev:
	dune exec dancelor -- --config assets/config.local.json

local:
	dune exec dancelor -- --config assets/config.local.json --write-storage

dev-test:
	dune exec dancelor -- --config tests/config.json

clean:
	dune clean
