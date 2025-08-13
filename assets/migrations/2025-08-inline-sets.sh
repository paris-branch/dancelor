#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.
## WARNING: This script is NOT idempotent. It will compute new ids every time..

################################################################################
################################################################################
################################################################################

make_id () {
    ## An id is a string of the form `0000-0000-0000` where characters are
    ## numbers or letters, and the “sum” of the characters is equal to 0 modulo
    ## 36.

    ## Generate 11 random characters from the set [0-9a-z], store them as
    ## `000-0000-0000` (three, then four, then four again).
    characters=$(cat /dev/urandom | tr -dc '0-9a-z' | fold -w 11 | head -n 1 | sed 's/^\(.\{3\}\)\(.\{4\}\)\(.\{4\}\)$/\1-\2-\3/')

    ## Calculate the missing character by adding the values of the characters
    ## (digits = 0-9, letters = 10-35) and taking the value that is the
    ## remainder of the sum divided by 36.
    checksum=$(echo "$characters" | sed 's/-//g' | sed 's/./&\n/g' | awk '{sum += (index("0123456789abcdefghijklmnopqrstuvwxyz", $1) - 1)} END {print (substr("0123456789abcdefghijklmnopqrstuvwxyz0", (360000000 - sum) % 36 + 1, 1))}')
    characters=$checksum$characters

    echo "$characters"
}

printf >&2 'Extract inline sets...\n'
total=$(ls -1 book/ | wc -l)
index=0
for book in book/*/meta.yaml; do
    index=$((index + 1))
    printf >&2 '\r[%d/%d] \033[K%s...' "$index" "$total" "$book"

    while :; do
        one_id=$(make_id)
        output=$(
            yq --output-format yaml --arg one_id "$one_id" '
                .["created-at"] as $created_at |
                .["modified-at"] as $modified_at |
                .privacy // "private" as $privacy |
                first(.contents[] | select(.[0] == "InlineSet"))[1] as $set |
                first(.contents[] | select(.[0] == "InlineSet")) |= (.[0] = "Set" | .[1] = $one_id) |
                {
                    book: .,
                    set: (
                        $set |
                        .["created-at"] = $created_at |
                        .["modified-at"] = $modified_at |
                        .privacy = $privacy
                    )
                }
            ' "$book"
        )
        if [ -z "$output" ]; then
            break
        else
            mkdir -p set/$one_id
            yq --output-format yaml '.set' <(echo "$output") > set/$one_id/meta.yaml
            yq --output-format yaml '.book' <(echo "$output") >| "$book"
        fi
    done
done
printf >&2 '\r[%d/%d] \033[Kdone.\n' "$total" "$total"
