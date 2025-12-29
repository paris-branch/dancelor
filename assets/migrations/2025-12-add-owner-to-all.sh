#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq -p gnugrep

set -euC

## WARNING: Call from the root of the repository.

################################################################################
################################################################################
################################################################################

printf >&2 'Grabbing owner from command line...\n'

owner_id=$(grep -rli "username:.*$1" user/ | sed 's|user/\([^/]*\)/meta.yaml|\1|') || true
if [ -z "$owner_id" ]; then owner_id=$1; fi
if ! owner_name=$(yq -r .username "user/$owner_id/meta.yaml" 2>/dev/null); then
    printf 'The user `%s` does not seem to exist, neither as an id or as a name.\n' "$owner_id"
    exit 2
fi

printf >&2 'Found user `%s` (%s).\n' "$owner_id" "$owner_name"

printf >&2 'Proceed? (y/n) '
read -r answer
if [ "$answer" != 'y' ]; then exit 1; fi

printf >&2 'Cleaning up empty directories...\n'

find -type d -exec rmdir {} + 2>/dev/null || :

printf >&2 'Adding an `owner` field to all objects in the database...\n'

for model in book dance person set source tune user version; do

    total=$(ls -1 $model/ | wc -l)
    index=0
    for elem in $(ls -1 $model/); do
        index=$((index + 1))
        printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$elem"

        yq --in-place --output-format yaml ".owner = \"$owner_id\"" "$model/$elem/meta.yaml"
    done
    printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"

done

printf >&2 'done.\n'
