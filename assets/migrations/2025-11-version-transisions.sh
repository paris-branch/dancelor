#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

printf >&2 'Make destructured versions definition cleaner...\n'

total=$(ls -1 version/ | wc -l)
index=0
for version in $(ls -1 version/); do
    index=$((index + 1))
    printf >&2 '\r  versions: [%d/%d] \033[K%s...' "$index" "$total" "$version"

    yq --in-place --output-format yaml '
        .content |= (
            if .[0] != "Destructured" then
                .
            else
                ["Destructured", (.[1] | .transitions = [])]
            end
        )
    ' "version/$version/meta.yaml"
done
printf >&2 '\r  versions: [%d/%d] \033[Kdone.\n' "$total" "$total"

printf >&2 'done.\n'
