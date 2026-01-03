#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq -p gnugrep

set -euC

## WARNING: Call from the root of the repository.

################################################################################

printf >&2 'Create the list of all users...\n'

first=true

users=$(
    printf '['
    for user in $(cd user/ && ls -1); do
        if $first; then
            first=false
            printf '"%s"' "$user"
        else
            printf ', "%s"' "$user"
        fi
    done
    printf ']'
)

echo "$users"

################################################################################

printf >&2 'Give visibility to all users to all objects in the database...\n'

for model in set book; do
    total=$(ls -1 $model/ | wc -l)
    index=0
    for elem in $(ls -1 $model/); do
        index=$((index + 1))
        printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$elem"

        yq --in-place --output-format yaml "
            .access.visibility = [\"Select_viewers\", $users]
        " "$model/$elem/meta.yaml"
    done
    printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"
done

printf >&2 'done.\n'

################################################################################
