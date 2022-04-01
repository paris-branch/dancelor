#/bin/sh
set -euC

## Find all files not in a .git directory that end in .ml or .mli and run
## `ocp-indent` on them. We do not run `ocp-indent` only .mll and .mly files
## because it tends to butcher them.
##
opam exec -- \
  find . -name .git -prune -o '(' -name '*.ml' -o -name '*.mli' ')' \
  -exec ocp-indent --inplace '{}' ';'

## Check if `git diff` is happy. If it is not, print the full diff.
##
if ! git diff --quiet; then
  echo '`ocp-indent` would want to change the following things:'
  git diff --color
  exit 1
fi
