.PHONY: build dev local dev-test clean

build:
	dune build @install

dev:
	dune exec --watch src/server/server.exe -- --config assets/config.local.json

local:
	dune exec src/server/server.exe -- --config assets/config.local.json --write-storage

dev-test:
	dune exec --watch src/server/server.exe -- --config tests/config.json

clean:
	dune clean
