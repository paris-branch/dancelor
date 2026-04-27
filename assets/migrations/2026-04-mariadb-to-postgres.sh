#!/usr/bin/env sh
set -euC

MARIADB_SOCK=$(realpath "$1")
PG_SOCK=$(realpath "$2")

LOAD_FILE=$(mktemp)

cat >| $LOAD_FILE <<EOF
LOAD DATABASE
  FROM mysql://unix:$MARIADB_SOCK:/dancelor
  INTO postgresql://unix:$PG_SOCK:5432/dancelor
;
EOF

nix run nixpkgs#pgloader -- "$LOAD_FILE"
