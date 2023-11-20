.PHONY: build docker ci doc release test local dev serve init-only check-tunes indent clean

DUNEJOBSARG :=
ifneq ($(DUNEJOBS),)
DUNEJOBSARG := -j $(DUNEJOBS)
endif

build:
	dune build $(DUNEJOBSARG) @install
	ln -sf _build/install/default/bin .

release:
	dune build $(DUNEJOBSARG) --profile=release @install
	ln -sf _build/install/default/bin .

docker:
	docker build --tag dancelor .

doc:
	dune build $(DUNEJOBSARG) @doc
	ln -sf _build/default/_doc/_html doc

test:
	dune test $(DUNEJOBSARG)

dev: build
	bin/dancelor --config assets/config.json --no-routines --no-sync-storage --no-write-storage

local: build
	bin/dancelor --config assets/config.json --no-routines --no-sync-storage

indent:
	opam exec -- \
	  find . -name .git -prune -o '(' -name '*.ml' -o -name '*.mli' ')' \
	  -exec ocp-indent --inplace '{}' ';'

clean:
	dune clean $(DUNEJOBSARG)
	rm -f bin doc
	rm -f share/static/dancelor
