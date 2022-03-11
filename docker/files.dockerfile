FROM dancelor_base

## FIXME: Once there is a `dancelor` user, we should change this `chown`.
COPY --chown=opam . .
