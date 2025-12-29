#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

################################################################################
################################################################################
################################################################################

printf >&2 'Invert user->person relation to person->user...\n'

total=$(ls -1 user/ | wc -l)
index=0
for user in $(ls -1 user/); do
    index=$((index + 1))
    printf >&2 '\r  users: [%d/%d] \033[K%s...' "$index" "$total" "$user"

    person=$(yq -r .person "user/$user/meta.yaml")
    yq --in-place --output-format yaml 'del(.person) | .privacy = "public"' "user/$user/meta.yaml"
    yq --in-place --output-format yaml ".user = \"$user\"" "person/$person/meta.yaml"
done
printf >&2 '\r  users: [%d/%d] \033[Kdone.\n' "$total" "$total"

printf >&2 'done.\n'
