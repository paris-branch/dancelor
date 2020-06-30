#!/bin/sh
set -uC ## no -e for this script

usage () {
    cat <<EOF
Usage: $0 [OPTIONS]

OPTIONS:

  --no-clean   only build and start
  --no-opam    pull but do not update OPAM
  --full       pull and update OPAM (default)

  --help  print this help and exit
EOF
}

rc=103

while [ $# -gt 0 ]; do
    case $1 in
        --no-clean) rc=101 ;;
        --no-opam)  rc=102 ;;
        --full)     rc=103 ;;

        --help)
            usage
            exit 0
            ;;

        *)
            printf 'Unknown argument: %s\n\n' "$1"
            usage
            exit 2
    esac
    shift
done

clean () { make clean; }
pull () { git pull; }
update_opam () { opam update; opam upgrade -y; }
build () { make release; }
serve () { bin/dancelor-server --config share/config.json; }

while :; do
    case $rc in
        101) build ;;

        102) clean; pull; build ;;

        103) clean; pull; update_opam; build ;;

        *) printf 'Unexpected return code `%d`. Restarting anyway.\n' "$rc"
    esac

    serve
    rc=$?
done
