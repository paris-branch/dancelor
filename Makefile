.PHONY: build docker doc release test local dev serve init-only check-tunes clean

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
	docker build -t dancelor_base - < docker/base.dockerfile
	docker build -t dancelor_deps  -f docker/deps.dockerfile .
	docker build -t dancelor       -f docker/build.dockerfile .

doc:
	dune build $(DUNEJOBSARG) @doc
	ln -sf _build/default/_doc/_html doc

test:
	dune test $(DUNEJOBSARG)

local: build
	bin/dancelor-server --config share/config.json --no-routines --no-sync-storage --no-write-storage

dev: build
	bin/dancelor-server --config share/config.json --no-routines --no-sync-storage

serve: release
	bin/dancelor-server --config share/config.json

init-only: release
	bin/dancelor-server --config share/config.json --init-only

check-tunes: build
	bin/dancelor-server --config share/config.json --heavy-routines --no-sync-storage --no-write-storage --loglevel info

clean:
	dune clean $(DUNEJOBSARG)
	rm -f bin doc
	rm -f share/static/dancelor
