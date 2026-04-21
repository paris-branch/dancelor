.PHONY: build dev entr local dev-test clean mariadb

build:
	dune build @install @runtest

entr: mariadb
	watchexec --clear --restart -- 'dune build @install @runtest && dune exec dancelor -- assets/config.dev.json'

dev: mariadb
	dune exec dancelor -- assets/config.dev.json

local: mariadb
	dune exec dancelor -- <(jq '.write_storage = true' assets/config.dev.json)

dev-test: mariadb
	dune exec dancelor -- <(jq '.database = "tests/database" | .loglevel = {cases: [], default: "warning"} | .sync_storage = false | .write_storage = false' assets/config.dev.json)

mariadb:
	@scripts/mariadb-start

clean:
	@scripts/mariadb-stop
	@scripts/mariadb-clean
	dune clean
