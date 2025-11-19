#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

printf >&2 'Add details to version sources...\n'

total=$(ls -1 version/ | wc -l)
index=0
for version in $(ls -1 version/); do
    index=$((index + 1))
    printf >&2 '\r  versions: [%d/%d] \033[K%s...' "$index" "$total" "$version"

    yq --in-place --output-format yaml '
        if .sources != null then
            .sources |= map ({source: .[0], structure: .[1]})
        end
    ' "version/$version/meta.yaml"
done
printf >&2 '\r  versions: [%d/%d] \033[Kdone.\n' "$total" "$total"

printf >&2 'done.\n'
