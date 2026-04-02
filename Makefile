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
	dune exec dancelor -- <(jq '.database = "tests/database" | .loglevel = {cases: [], default: "warning"} | .sync_storage = false | .write_storage = false' assets/config.dev.json)

clean:
	dune clean
