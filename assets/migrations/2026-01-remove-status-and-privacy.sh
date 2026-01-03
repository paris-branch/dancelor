#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq -p gnugrep

set -euC

## WARNING: Call from the root of the repository.

################################################################################

printf >&2 'Removing status and privacy to all objects in the database...\n'

for model in source version tune dance person set book user; do
    total=$(ls -1 $model/ | wc -l)
    index=0
    for elem in $(ls -1 $model/); do
        index=$((index + 1))
        printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$elem"

        yq --in-place --output-format yaml "
            .meta |= (del(.status) | del(.privacy))
        " "$model/$elem/meta.yaml"
    done
    printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"
done

printf >&2 'done.\n'

################################################################################
