#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq -p gnugrep

set -euC

## WARNING: Call from the root of the repository.

printf >&2 'Grabbing new admin from command line...\n'

admin_id=$(grep -rli "username:.*$1" user/ | sed 's|user/\([^/]*\)/meta.yaml|\1|') || true
if [ -z "$admin_id" ]; then admin_id=$1; fi
if ! admin_name=$(yq -r .value.username "user/$admin_id/meta.yaml" 2>/dev/null); then
    printf 'The user `%s` does not seem to exist, neither as an id or as a name.\n' "$admin_id"
    exit 2
fi

printf >&2 'Found user `%s` (%s).\n' "$admin_id" "$admin_name"

printf >&2 'Proceed? (y/n) '

read -r answer
if [ "$answer" != 'y' ]; then exit 1; fi

################################################################################

printf >&2 'Create a map from persons to users...\n'

mapping=$(mktemp)

{
first=true
printf '{\n'

total=$(ls -1 person/ | wc -l)
index=0
for person in $(ls -1 person/); do
    index=$((index + 1))
    printf >&2 '\r  [%d/%d] \033[K%s...' "$index" "$total" "$person"

    user_id=$(yq -r .value.user "person/$person/meta.yaml")
    if [ "$user_id" != null ]; then
        $first && first=false || printf ', '
        printf '"%s": "%s"\n' "$person" "$user_id"
    fi
done

printf '}\n'
} >| $mapping

printf >&2 'done\n'

cat "$mapping"

################################################################################

printf >&2 'Replacing the `owners` field to all objects in the database...\n'

for model in set book; do
    [ "$model" = book ] && conceptors=authors || conceptors=conceptors

    total=$(ls -1 $model/ | wc -l)
    index=0
    for elem in $(ls -1 $model/); do
        index=$((index + 1))
        printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$elem"

        yq --in-place --output-format yaml --argjson mapping "$(cat "$mapping")" "
            .access |= (
                del(.owner) |
                .owners = (
                    ([(.value.$conceptors // [])[] | \$mapping[.] // empty] | unique) as \$mapped |
                    if (\$mapped | length) > 0 then
                        \$mapped
                    else
                        [\"$admin_id\"]
                    end
                )
            )
        " "$model/$elem/meta.yaml"
    done
    printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"

done

printf >&2 'done.\n'

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
