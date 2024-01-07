.PHONY: build doc test local dev indent clean

DUNEJOBSARG :=
ifneq ($(DUNEJOBS),)
DUNEJOBSARG := -j $(DUNEJOBS)
endif

build:
	dune build $(DUNEJOBSARG) @install
	ln -sf _build/install/default/bin .

doc:
	dune build $(DUNEJOBSARG) @doc
	ln -sf _build/default/_doc/_html doc

test:
	dune test $(DUNEJOBSARG)

dev: build
	bin/dancelor --config assets/config.local.json

local: build
	bin/dancelor --config assets/config.local.json --write-storage

indent:
	opam exec -- \
	  find . -name .git -prune -o '(' -name '*.ml' -o -name '*.mli' ')' \
	  -exec ocp-indent --inplace '{}' ';'

clean:
	dune clean $(DUNEJOBSARG)
	rm -f bin doc
	rm -f share/static/dancelor
