#!/bin/sh
set -uC ## no -e for this script

build () {
    make clean
    git pull
    make release
}

serve () {
    bin/dancelor-server --config share/config.json
}

rc=102

while :; do
    case $rc in
        101) : ;;

        102) build ;;

        *)
            printf 'Unexpected return code `%d`. Exiting.\n' "$rc"
            exit 1
            ;;
    esac

    serve
    rc=$?
done
