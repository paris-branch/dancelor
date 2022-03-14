FROM dancelor_build

RUN opam exec -- make test
