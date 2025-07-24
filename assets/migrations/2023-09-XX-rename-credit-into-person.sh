#!/bin/sh
set -euC

if [ -e person ]; then
    printf >&2 'The file `person` already exist. Refusing to proceed.\n'
    exit 2
fi

printf 'Moving `credit/` to `person/`...\n'
mv credit/ person/
git add credit person
git commit -m 'Move `credit/` to `person/`'
printf 'done.\n\n'

printf 'Renaming field `line` into `name`...\n'
find person/ -name meta.yaml -exec \
    yq -iy 'with_entries(if .key == "line" then .key = "name" else . end)' \
    {} +
git add person/
git commit -m 'Rename field `line` into `name`'
printf 'done.\n'
