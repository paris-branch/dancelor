#!/bin/sh
set -euC

usage () {
    cat <<EOF
Usage: $0 [OPTIONS]

OPTIONS:
  --no-pull    build
  --no-clean   pull and build
  --no-opam    clean, pull, and build
  --full       clean, pull, update OPAM, and build (default)

  --help  print this help and exit
EOF
}

rc=104

while [ $# -gt 0 ]; do
    case $1 in
        --no-pull)  rc=101 ;;
        --no-clean) rc=102 ;;
        --no-opam)  rc=103 ;;
        --full)     rc=104 ;;

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

        102) pull; build ;;

        103) clean; pull; build ;;

        104) clean; pull; update_opam; build ;;

        *) printf 'Unexpected return code `%d`. Restarting anyway.\n' "$rc"
    esac

    serve && :
    rc=$?
done
