.PHONY: build dev-test local dev entr indent clean

DUNEJOBSARG :=
ifneq ($(DUNEJOBS),)
DUNEJOBSARG := -j $(DUNEJOBS)
endif

build:
	dune build $(DUNEJOBSARG) @install
	ln -sf _build/install/default/bin .

## The release profile will, among other things, eliminate dead code and minify
## `client.js` and `server.exe`. The Nix module already does that.
release:
	dune build $(DUNEJOBSARG) --profile release @install
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
	dune clean $(DUNEJOBSARG)
	rm -f bin
	rm -f share/static/dancelor
