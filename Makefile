.PHONY: build run entr clean postgres

build:
	dune build @install @runtest

run: postgres
	dune exec dancelor -- assets/config.dev.json

entr: postgres
	watchexec --clear --restart -- 'dune build @install @runtest && dune exec dancelor -- assets/config.dev.json'

postgres:
	@scripts/postgres-start

clean:
	@scripts/postgres-reset
	dune clean
