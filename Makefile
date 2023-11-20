.PHONY: build docker ci doc release test local dev serve init-only check-tunes indent clean

DUNEJOBSARG :=
ifneq ($(DUNEJOBS),)
DUNEJOBSARG := -j $(DUNEJOBS)
endif

build:
	cd share/static/style && sassc style.scss ../style.css
	dune build $(DUNEJOBSARG) @install
	ln -sf _build/install/default/bin .
	ln -sf ../../_build/install/default/share/dancelor share/static/

release:
	cd share/static/style && sassc style.scss ../style.css
	dune build $(DUNEJOBSARG) --profile=release @install
	ln -sf _build/install/default/bin .
	ln -sf ../../_build/install/default/share/dancelor share/static/

docker:
	docker build --tag dancelor .

ci:
	docker build --tag dancelor_base  --target base  .
	docker build --tag dancelor_files --target files .
	docker build --tag dancelor_deps  --target deps  .
	docker build --tag dancelor_build --target build .
	docker run dancelor_build opam exec -- make test
	docker run dancelor_files ci/indent.sh
	docker run dancelor_files ci/opam-lint.sh
	docker run dancelor_files ci/opam-diff.sh

doc:
	dune build $(DUNEJOBSARG) @doc
	ln -sf _build/default/_doc/_html doc

test:
	dune test $(DUNEJOBSARG)

local: build
	bin/dancelor-server --config assets/config.json --no-routines --no-sync-storage --no-write-storage

dev: build
	bin/dancelor-server --config assets/config.json --no-routines --no-sync-storage

serve: release
	bin/dancelor-server --config assets/config.json

init-only: release
	bin/dancelor-server --config assets/config.json --init-only

check-tunes: build
	bin/dancelor-server --config assets/config.json --heavy-routines --no-sync-storage --no-write-storage --loglevel info

indent:
	opam exec -- \
	  find . -name .git -prune -o '(' -name '*.ml' -o -name '*.mli' ')' \
	  -exec ocp-indent --inplace '{}' ';'

clean:
	dune clean $(DUNEJOBSARG)
	rm -f bin doc
	rm -f share/static/dancelor
