FROM dancelor_deps

## FIXME: Once there is a `dancelor` user, we should change this `chown`.
COPY --chown=opam . .
RUN opam exec -- make
