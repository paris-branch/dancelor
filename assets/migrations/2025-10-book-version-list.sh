#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

################################################################################
################################################################################
################################################################################

printf >&2 'Allowing multiple versions in book pages...\n'

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
                if $type == "Version" then
                    [
                        "Versions",
                        [[$id, $params]]
                    ]
                else
                    if $type == "Dance" and $params[0] == "DanceVersion" then
                        [
                            "Dance",
                            $id,
                            [
                                "DanceVersions",
                                [[$params[1], $params[2]]]
                            ]
                        ]
                    else
                        .
                    end
                end
            )
        )
    ' "book/$book/meta.yaml"
done
printf >&2 '\r  books: [%d/%d] \033[Kdone.\n' "$total" "$total"

printf >&2 'done.\n'
