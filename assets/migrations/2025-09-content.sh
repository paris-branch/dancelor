#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yq

set -euC

## WARNING: Call from the root of the repository.

printf >&2 'Integrate version contents...\n'
(
    cd version
    total=$(ls -1 | wc -l)
    index=0
    for entry in *; do
        index=$((index + 1))
        printf >&2 '\r[%d/%d] \033[K%s...' "$index" "$total" "$entry"

        yq \
          --in-place --output-format yaml \
          --rawfile content "$entry/content.ly" \
          '
              .bars as $bars | .structure as $structure |
              del(.bars) | del(.structure) |
              .content = ["Monolithic", {bars: $bars, structure: $structure, lilypond: $content}] |
              if .sources != null then .sources |= map([., $structure]) end
          ' \
          "$entry/meta.yaml"
        rm "$entry/content.ly"
    done
    printf >&2 '\r[%d/%d] \033[Kdone.\n' "$total" "$total"
)
printf >&2 'done.\n'
