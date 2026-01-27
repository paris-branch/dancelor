#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

printf >&2 'Add details to tune composers...\n'

total=$(ls -1 tune/ | wc -l)
index=0
for tune in $(ls -1 tune/); do
    index=$((index + 1))
    printf >&2 '\r  tunes: [%d/%d] \033[K%s...' "$index" "$total" "$tune"

    yq --in-place --output-format yaml '
        if .value.composers != null then
            .value.composers |= map ({composer: ., details: ""})
        end
    ' "tune/$tune/meta.yaml"
done
printf >&2 '\r  tunes: [%d/%d] \033[Kdone.\n' "$total" "$total"

printf >&2 'done.\n'
