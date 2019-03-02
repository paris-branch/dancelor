#!/bin/sh
set -euC

serve () {
    make clean
    git pull
    make serve || true
}

while true; do
    serve
done
