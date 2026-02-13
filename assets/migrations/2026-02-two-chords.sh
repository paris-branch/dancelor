#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq -p gnugrep

set -euC

## WARNING: Call from the root of the repository.

################################################################################

printf >&2 'Updating `two_chords` fields in all the dances...\n'

for model in dance; do
    total=$(ls -1 $model/ | wc -l)
    index=0
    for elem in $(ls -1 $model/); do
        index=$((index + 1))
        printf >&2 '\r  %ss: [%d/%d] \033[K%s...' "$model" "$index" "$total" "$elem"

        yq --in-place --output-format yaml '
            if .value.["two-chords"] == true then
                .value.["two-chords"] = ["Two_chords"]
            else if .value.["two-chords"] == false then
                .value.["two-chords"] = ["One_chord"]
            end
            end
        ' "$model/$elem/meta.yaml"
    done
    printf >&2 '\r  %ss: [%d/%d] \033[Kdone.\n' "$model" "$total" "$total"
done

printf >&2 'done.\n'

################################################################################
