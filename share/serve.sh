#!/bin/sh
set -euC

serve () {
    make clean

    git pull
    opam install . --deps-only --yes

    (
	cd ../dancelor-database
	git pull --rebase
	git push
    )

    make serve || true
}

while true; do
    serve
done
