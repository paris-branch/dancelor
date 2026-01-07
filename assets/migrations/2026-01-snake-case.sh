#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq -p gnugrep

set -euC

## WARNING: Call from the root of the repository.

################################################################################

printf >&2 'Renaming constructors to snake_case in the books...\n'

for model in book; do
    total=$(ls -1 $model/ | wc -l)
    index=0
    for elem in $(ls -1 $model/); do
        index=$((index + 1))
        printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$elem"

        yq --in-place --output-format yaml '
            .value.contents |= map(
                if .[0] == "Dance" then
                    [
                        .[0],
                        .[1],
                        (
                            if .[2][0] == "DanceOnly" then
                                ["Dance_only"]
                            elif .[2][0] == "DanceVersions" then
                                ["Dance_versions", .[2][1]]
                            elif .[2][0] == "DanceSet" then
                                ["Dance_set", .[2][1], .[2][2]]
                            else
                                .[2]
                            end
                        )
                    ]
                else
                    .
                end
            )
        ' "$model/$elem/meta.yaml"
    done
    printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"
done

printf >&2 'done.\n'

################################################################################
