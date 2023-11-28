#!/bin/sh
set -euC

usage () {
    cat <<EOF
Usage: $0 [OPTIONS]

Wrapper around 'dancelor-server' to handle the environment, logging and
restarting (via '/victor').

OPTIONS:
  --no-pull         build
  --no-clean        pull and build
  --no-opam         clean, pull, and build
  --full            clean, pull, install deps, and build (default)
  --log-dir DIR     a directory where to put log files
  --help            print this help and exit
EOF
}

rc=104
log_dir=

while [ $# -gt 0 ]; do
    case $1 in
        --no-pull)  rc=101 ;;
        --no-clean) rc=102 ;;
        --no-opam)  rc=103 ;;
        --full)     rc=104 ;;

        --log-dir)
            shift
            log_dir=$1
            ;;

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
opam_deps () {
    opam update
    dune build &>/dev/null || true
    opam install . --deps-only
}
build () { make release; }

just_serve () {
    bin/dancelor --config share/config.json
}
serve () {
    if [ -z "$log_dir" ]; then
        just_serve
    else
        log_file=$log_dir/$(date +'%Y-%m-%d %H:%M:%S').log
        ## `stdbuf -i0` runs the given command with unbuffered input stream,
        ## which is necessary to ensure that `tee` prints to the file not only
        ## when Dancelor exits.
        just_serve 2>&1 | stdbuf -i0 tee "$log_file"
    fi
}

while :; do
    case $rc in
        101) build ;;

        102) pull; build ;;

        103) clean; pull; build ;;

        104) clean; pull; opam_deps; build ;;

        *) printf 'Unexpected return code `%d`. Restarting anyway.\n' "$rc"
    esac

    serve && :
    rc=$?
done
