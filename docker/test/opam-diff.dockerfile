FROM dancelor_files

## FIXME: This test does not complain about OPAM files that did not exist before
## being generated.

## For each `(name ...)` statement in `dune-project`:
## - compute the name of the corresponding OPAM file,
## - have Dune generate it,
## - check for changes.
RUN cat dune-project                                                           \
      | sed 's|\((name [^)]*)\)|\n\1\n|g'                                      \
      | sed -n 's|^(name \(.*\))$|\1.opam|p'                                   \
      | while read -r opam_file; do                                            \
          opam exec -- dune build "$opam_file";                                \
          if ! git diff --quiet "$opam_file"; then                             \
            echo "$opam_file: differs before and after build:";                \
            git diff --color "$opam_file";                                     \
            exit 1;                                                            \
          fi;                                                                  \
        done
