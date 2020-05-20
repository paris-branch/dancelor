#!/bin/sh
set -uC ## no -e for this script

clean () { make clean; }
pull () { git pull; }
update_opam () { opam update; opam upgrade -y; }
build () { make release; }
serve () { bin/dancelor-server --config share/config.json; }

rc=103

while :; do
    case $rc in
        101) : ;;

        102) clean; pull; build ;;

        103) clean; pull; update_opam; build ;;

        *)
            printf 'Unexpected return code `%d`. Exiting.\n' "$rc"
            exit 1
            ;;
    esac

    serve
    rc=$?
done
