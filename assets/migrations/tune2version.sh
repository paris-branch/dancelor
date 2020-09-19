#!/bin/sh
set -euC

find src -type f | \
    while read -r file; do
        sed -i \
            -e 's|version|lyversion|g' \
            -e 's|TuneGroup|Tvne|g' \
            -e 's|tune_group|tvne|g' \
            -e 's|tune-group|tvne|g' \
            -e 's|Tune|Version|g' \
            -e 's|tune|version|g' \
            -e 's|Tvne|Tune|g' \
            -e 's|tvne|tune|g' \
            "$file"
    done

move_files_sed () {
    find src -type f | \
        while read -r file; do
            ofile=$(echo "$file" | sed "$1")
            [ "$ofile" = "$file" ] || { mkdir -p "$(dirname "$ofile")" && mv "$file" "$ofile"; }
        done
}

mv cache/tune cache/version

move_files_sed 's|version|lyversion|'
move_files_sed 's|tuneGroup|tvne|'
move_files_sed 's|tune|version|'
move_files_sed 's|tvne|tune|'

exit 0
