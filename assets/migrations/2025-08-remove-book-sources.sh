#!/bin/sh

set -euC

## WARNING: Call from the root of the repository.

printf >&2 'Remove sources from books...\n'
find book -name meta.yaml -exec sed -i 's|^source:.*$||' -i {} +
printf >&2 'done.\n'
