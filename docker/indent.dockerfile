FROM dancelor_base

RUN opam install --yes ocp-indent

## FIXME: Once there is a `dancelor` user, we should change this `chown`.
COPY --chown=opam *.opam .

RUN git status
RUN git diff

RUN find src lib '(' -name '*.ml' -o -name '*.ml?' ')' \
         -exec ocp-indent --inplace '{}' ';'

RUN git status
RUN git diff
