#!/bin/sh
set -euC

if [ -n "$(opam lint --short)" ]; then
  opam lint
  exit 1
fi
