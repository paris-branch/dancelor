#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq -p gnugrep

set -euC

## WARNING: Call from the root of the repository.

################################################################################

printf >&2 'Replacing access.visibility by access.meta_visibility...\n'

for model in set book; do
    total=$(ls -1 $model/ | wc -l)
    index=0
    for elem in $(ls -1 $model/); do
        index=$((index + 1))
        printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$elem"

        yq --in-place --output-format yaml "
            .access |= (
                .meta_visibility = (if .visibility != null then .visibility else [\"Owners_only\"] end) |
                del(.visibility)
            )
        " "$model/$elem/meta.yaml"
    done
    printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"
done

printf >&2 'done.\n'

################################################################################
