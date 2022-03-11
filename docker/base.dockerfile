FROM ocaml/opam:debian-10-ocaml-4.12

USER root

## FIXME: When OPAM 2.1 becomes the default, this will no longer be necessary.
RUN ln -sf /usr/bin/opam-2.1 /usr/bin/opam

## FIXME: Add a user `dancelor` and make sure that it can write to
## `/home/opam/.opam`.

## FIXME: Hack on `/bin/sh` to get it to `eval $(opam env)` before actually
## starting. It is supposed to read `~/.profile` but somehow that does not seem
## to be the case.

WORKDIR /wd
RUN chown -R opam:opam /wd

USER opam

ARG JOBS=2

ENV OPAMJOBS=$JOBS
ENV DUNEJOBS=$JOBS
