FROM dancelor_base

## FIXME: Once there is a `dancelor` user, we should change this `chown`.
COPY --chown=opam *.opam .
RUN opam install . --yes --depext-only
RUN opam install . --yes --deps-only --with-test --with-doc
