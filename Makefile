.PHONY: build dev entr local dev-test clean

build:
	dune build @install @runtest

entr:
	watchexec --clear --restart -- 'dune build @install @runtest && dune exec dancelor -- assets/config.dev.json'

dev:
	dune exec dancelor -- assets/config.dev.json

local:
	dune exec dancelor -- <(jq '.write_storage = true' assets/config.dev.json)

dev-test:
	dune exec dancelor -- tests/config.json

clean:
	dune clean
