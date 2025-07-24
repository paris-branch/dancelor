#!/bin/sh
set -euC

printf 'I will start by adding `created-at` and `modified-at`\n'
printf 'to all the `meta.yaml` files.\n'

files=$(find . -name 'meta.yaml' -exec dirname '{}' ';')
total=$(echo "$files" | wc -l)
printf 'Handling %d entries.\n' $total

index=0
for entry in $files; do
    index=$((index + 1))
    printf '\r[%d/%d] %s                    ' $index $total $entry

    created_at=$(git log --format='%cI' "$entry" | tail -n 1)
    modified_at=$(git log --format='%cI' "$entry" | head -n 1)
    echo "created-at: $created_at" >> "$entry"/meta.yaml
    echo "modified-at: $modified_at" >> "$entry"/meta.yaml
done
printf '\nDone!\n'

################################################################################

printf 'I will now add `created-at` and `modified-at`\n'
printf 'to all inline sets in books.\n'

files=$(find book -name 'meta.yaml' -exec dirname '{}' ';')
total=$(echo "$files" | wc -l)
printf 'Handling %d entries.\n' $total

index=0
for entry in $files; do
    index=$((index + 1))
    printf '\r[%d/%d] %s                    ' $index $total $entry

    created_at=$(git log --format='%cI' "$entry" | tail -n 1)
    modified_at=$(git log --format='%cI' "$entry" | head -n 1)

    sed -i "s|^    - name: \(.*\)$|    - name: \1\n      created-at: $created_at\n      modified-at: $modified_at\n|" $entry/meta.yaml
done
printf '\nDone!\n'
