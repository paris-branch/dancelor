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

printf >&2 'Removing existing mapping file... '
rm -f mapping-to-ids.yaml mapping-to-ids.json
printf >&2 'done.\n'

printf >&2 'Creating new mapping file...\n'
for model in book dance person set source tune user version; do
    (
        cd "$model"
        printf '%s:\n' "$model"
        total=$(ls -1 | wc -l)
        index=0
        for entry in *; do
            index=$((index + 1))
            printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$entry"
            printf '  "%s": "%s"\n' "$entry" "$(make_id)"
        done
        printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"
    )
done > mapping-to-ids.yaml
printf >&2 'done.\n'

printf >&2 'Checking mapping file... '
duplicate_ids=$(yq --raw-output 'to_entries | map(.value) | group_by(.) | map(select(length > 1)) | map(.[0]) | .[]' mapping-to-ids.yaml)
if [ -n "$duplicate_ids" ]; then
    printf >&2 '\nDuplicate ids found:\n%s\n' "$duplicate_ids"
    exit 3
fi
printf >&2 'done.\n'

printf >&2 'Converting mapping file to JSON... '
yq --output-format json . mapping-to-ids.yaml > mapping-to-ids.json
printf >&2 'done.\n'

################################################################################
################################################################################
################################################################################

printf >&2 'Changing slugs to ids...\n'

total=$(ls -1 book/ | wc -l)
index=0
for book in $(ls -1 book/); do
    index=$((index + 1))
    printf >&2 '\r  books: [%d/%d] \033[K%s...' "$index" "$total" "$book"

    yq --in-place --output-format yaml --rawfile ids mapping-to-ids.json '
        ($ids | fromjson) as $ids |
        if .authors != null then .authors |= map($ids.person.[.]) end |
        .contents |= map(
            if .[0] == "Version" then (
                .[1] |= $ids.version.[.] |
                .[2] |= (if .["for-dance"] != null then .["for-dance"] |= $ids.dance.[.] end)
            ) elif .[0] == "Set" then (
                .[1] |= $ids.set.[.] |
                .[2] |= (if .["for-dance"] != null then .["for-dance"] |= $ids.dance.[.] end)
            ) elif .[0] == "InlineSet" then (
                .[1] |= (
                    if .conceptors != null then .conceptors |= map($ids.person.[.]) end |
                    if .dances != null then .dances |= map($ids.dance.[.]) end |
                    if .["versions-and-parameters"] != null then (
                        .["versions-and-parameters"] |= map(
                            .[0] |= $ids.version.[.] |
                            .[1] |= (if .["for-dance"] != null then .["for-dance"] |= $ids.dance.[.] end)
                        )
                    ) end
                ) |
                .[2] |= (if .["for-dance"] != null then .["for-dance"] |= $ids.dance.[.] end)
            ) end
        )
    ' "book/$book/meta.yaml"
done
printf >&2 '\r  books: [%d/%d] \033[Kdone.\n' "$total" "$total"

total=$(ls -1 dance/ | wc -l)
index=0
for dance in $(ls -1 dance/); do
    index=$((index + 1))
    printf >&2 '\r  dances: [%d/%d] \033[K%s...' "$index" "$total" "$dance"

    yq --in-place --output-format yaml --rawfile ids mapping-to-ids.json '
        ($ids | fromjson) as $ids |
        if .devisers != null then .devisers |= map($ids.person.[.]) end
    ' "dance/$dance/meta.yaml"
done
printf >&2 '\r  dances: [%d/%d] \033[Kdone.\n' "$total" "$total"

total=$(ls -1 set/ | wc -l)
index=0
for set in $(ls -1 set/); do
    index=$((index + 1))
    printf >&2 '\r  sets: [%d/%d] \033[K%s...' "$index" "$total" "$set"

    yq --in-place --output-format yaml --rawfile ids mapping-to-ids.json '
        ($ids | fromjson) as $ids |
        if .conceptors != null then .conceptors |= map($ids.person.[.]) end |
        if .dances != null then .dances |= map($ids.dance.[.]) end |
        if .["versions-and-parameters"] != null then (
            .["versions-and-parameters"] |= map(
                .[0] |= $ids.version.[.] |
                .[1] |= (if .["for-dance"] != null then .["for-dance"] |= $ids.dance.[.] end)
            )
        ) end
    ' "set/$set/meta.yaml"
done
printf >&2 '\r  sets: [%d/%d] \033[Kdone.\n' "$total" "$total"

total=$(ls -1 source/ | wc -l)
index=0
for source in $(ls -1 source/); do
    index=$((index + 1))
    printf >&2 '\r  sources: [%d/%d] \033[K%s...' "$index" "$total" "$source"

    yq --in-place --output-format yaml --rawfile ids mapping-to-ids.json '
        ($ids | fromjson) as $ids |
        if .editors != null then .editors |= map($ids.person.[.]) end
    ' "source/$source/meta.yaml"
done
printf >&2 '\r  sources: [%d/%d] \033[Kdone.\n' "$total" "$total"

total=$(ls -1 tune/ | wc -l)
index=0
for tune in $(ls -1 tune/); do
    index=$((index + 1))
    printf >&2 '\r  tunes: [%d/%d] \033[K%s...' "$index" "$total" "$tune"

    yq --in-place --output-format yaml --rawfile ids mapping-to-ids.json '
        ($ids | fromjson) as $ids |
        if .composers != null then .composers |= map($ids.person.[.]) end |
        if .dances != null then .dances |= map($ids.dance.[.]) end
    ' "tune/$tune/meta.yaml"
done
printf >&2 '\r  tunes: [%d/%d] \033[Kdone.\n' "$total" "$total"

total=$(ls -1 user/ | wc -l)
index=0
for user in $(ls -1 user/); do
    index=$((index + 1))
    printf >&2 '\r  users: [%d/%d] \033[K%s...' "$index" "$total" "$user"

    yq --in-place --output-format yaml --rawfile ids mapping-to-ids.json '
        ($ids | fromjson) as $ids |
        .username = .["display-name"] |
        del(.["display-name"]) |
        if .person != null then .person |= $ids.person.[.] end
    ' "user/$user/meta.yaml"
done
printf >&2 '\r  users: [%d/%d] \033[Kdone.\n' "$total" "$total"

total=$(ls -1 version/ | wc -l)
index=0
for version in $(ls -1 version/); do
    index=$((index + 1))
    printf >&2 '\r  versions: [%d/%d] \033[K%s...' "$index" "$total" "$version"

    yq --in-place --output-format yaml --rawfile ids mapping-to-ids.json '
        ($ids | fromjson) as $ids |
        if .tune != null then .tune |= $ids.tune.[.] end |
        if .arrangers != null then .arrangers |= map($ids.person.[.]) end |
        if .sources != null then .sources |= map($ids.source.[.]) end
    ' "version/$version/meta.yaml"
done
printf >&2 '\r  versions: [%d/%d] \033[Kdone.\n' "$total" "$total"

printf >&2 'done.\n'

################################################################################
################################################################################
################################################################################

printf >&2 'Apply new mapping file...\n'
for model in book dance person set source tune user version; do
    (
        cd "$model"
        total=$(ls -1 | wc -l)
        index=0
        for entry in *; do
            index=$((index + 1))
            printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$entry"

            mv ./"$entry" ./"$(yq --raw-output ".[\"$model\"].[\"$entry\"]" ../mapping-to-ids.yaml)"
        done
        printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"
    )
done
printf >&2 'done.\n'
