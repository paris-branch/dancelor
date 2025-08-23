#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

################################################################################
################################################################################
################################################################################

printf >&2 'Changing `for-dance` parameters to dance pages...\n'

total=$(ls -1 book/ | wc -l)
index=0
for book in $(ls -1 book/); do
    index=$((index + 1))
    printf >&2 '\r  books: [%d/%d] \033[K%s...' "$index" "$total" "$book"

    yq --in-place --output-format yaml '
        .contents |= map (
            .[0] as $type |
            .[1] as $id |
            .[2] as $params |
            (
                if $params["for-dance"] != null then
                    [
                        "Dance",
                        $params["for-dance"],
                        [
                            (if $type == "Version" then "DanceVersion" else "DanceSet" end),
                            $id,
                            ($params | del(.["for-dance"]))
                        ]
                    ]
                else
                    .
                end
            )
        )
    ' "book/$book/meta.yaml"
done
printf >&2 '\r  books: [%d/%d] \033[Kdone.\n' "$total" "$total"

printf >&2 'done.\n'
