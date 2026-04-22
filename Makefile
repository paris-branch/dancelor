.PHONY: build run entr clean mariadb

build:
	dune build @install @runtest

run: mariadb
	dune exec dancelor -- assets/config.dev.json

entr: mariadb
	watchexec --clear --restart -- 'dune build @install @runtest && dune exec dancelor -- assets/config.dev.json'

mariadb:
	@scripts/mariadb-start

clean:
	@scripts/mariadb-stop
	@scripts/mariadb-clean
	dune clean
