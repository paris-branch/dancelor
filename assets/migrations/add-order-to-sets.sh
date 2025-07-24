#!/bin/sh
set -euC

cd set

number_of_repeats () {
    cat "$1" | jq -r .kind | sed 's|^\([0-9][0-9]*\)[^0-9].*$|\1|'
}

number_of_versions () {
    cat "$1" | jq '.["versions-and-parameters"] | length'
}

seq_comma () {
    seq "$@" | paste -sd,
}

for meta in */meta.json; do
    repeats=$(number_of_repeats "$meta")
    versions=$(number_of_versions "$meta")

    ## Deduce the order if you can.

    if [ $versions = $repeats ]; then
        order=$(seq_comma $versions)
    elif [ $versions = $(($repeats - 1)) ]; then
        order=$(seq_comma $versions),1
    elif [ $((2 * $versions)) = $repeats ]; then
        order=$(seq_comma $versions),$(seq_comma 2 $versions),1
    else
        printf >&2 '%s: %d versions and %d repeats\n' $meta $versions $repeats
        continue
    fi

    ## Add the order to the meta file

    {
        cat "$meta" | head -n -1
        printf ', "order": "%s"\n}' "$order"
    } \
        > "$meta".new
    mv "$meta".new "$meta"
done
