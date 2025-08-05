#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

printf >&2 'Rewrite dance names...\n'
(
    cd dance
    total=$(ls -1 | wc -l)
    index=0
    for entry in *; do
        index=$((index + 1))
        printf >&2 '\r[%d/%d] \033[K%s...' "$index" "$total" "$entry"

        yq --in-place --output-format yaml '.names = [.name] | del(.["name"])' "$entry/meta.yaml"
    done
    printf >&2 '\r[%d/%d] \033[Kdone.\n' "$total" "$total"
)
printf >&2 'done.\n'
