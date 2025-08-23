#!/bin/sh

set -euC

## WARNING: Call from the root of the repository.

printf >&2 'Remove short titles from books...\n'
find book -name meta.yaml -exec sed -i 's|^short-title:.*$||' -i {} +
printf >&2 'done.\n'
