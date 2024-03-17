.PHONY: build doc test tests unit-tests system-tests dev-test local dev entr indent clean

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

test: tests
tests:
	@echo 'You probably mean use the targets `unit-tests` or `system-tests`.'

unit-tests:
	dune test $(DUNEJOBSARG) --force

system-tests: build
	bin/dancelor --config tests/config.json --pid-file tests/run.pid &
	pytest -n auto -v || rc=$$?; \
	kill $$(cat tests/run.pid); rm tests/run.pid; \
	exit $$rc

dev: build
	bin/dancelor --config assets/config.local.json

entr:
	find src/ -type f | entr -ccrd make dev

local: build
	bin/dancelor --config assets/config.local.json --write-storage

dev-test: build
	bin/dancelor --config tests/config.json

indent:
	opam exec -- \
	  find . -name .git -prune -o '(' -name '*.ml' -o -name '*.mli' ')' \
	  -exec ocp-indent --inplace '{}' ';'

clean:
	dune clean $(DUNEJOBSARG)
	rm -f bin doc
	rm -f share/static/dancelor
