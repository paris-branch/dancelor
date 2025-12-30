#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

################################################################################
################################################################################
################################################################################

printf >&2 'Cleaning up empty directories...\n'

find -type d -exec rmdir {} + 2>/dev/null || :

printf >&2 'Split fields of the database into value/meta/access...\n'

for model in book dance person set source tune user version; do

    total=$(ls -1 $model/ | wc -l)
    index=0
    for elem in $(ls -1 $model/); do
        index=$((index + 1))
        printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$elem"

        yq --in-place --output-format yaml '
            {
                value: with_entries(
                    select(.key | IN("status", "privacy", "created-at", "modified-at", "owner") | not)
                ),
                meta: {
                    status,
                    privacy,
                    "created-at": ."created-at",
                    "modified-at": ."modified-at"
                } | with_entries(select(.value != null)),
                access: {
                    owner
                } | with_entries(select(.value != null))
            }
        ' "$model/$elem/meta.yaml"
    done
    printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"

done

printf >&2 'done.\n'
