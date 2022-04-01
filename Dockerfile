################################################################################
##   ___
##  | _ ) __ _ ___ ___
##  | _ \/ _` (_-</ -_)
##  |___/\__,_/__/\___|
##
##  The `base` stage prepares the base working image. It is a Debian with OPAM
##  and a few chosen packages. It contains a few hacks to setup the environment
##  properly. All images except the final one will be based on this one. It does
##  not depend in any way on the files in the outside.

FROM ocaml/opam:alpine-3.15-ocaml-4.14 AS base

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

RUN opam install --yes dune ocp-indent

ARG JOBS=2
ENV OPAMJOBS=$JOBS
ENV DUNEJOBS=$JOBS

################################################################################
##   ___ _ _
##  | __(_) |___ ___
##  | _|| | / -_|_-<
##  |_| |_|_\___/__/
##
##  The `files` stage is not per se necessary and is actually not used by any
##  other stage. It is useful in CI, however, for tests that only need the few
##  packages of `base` and the `files` to run tests (eg. `opam lint`).

FROM base AS files

## FIXME: Once there is a `dancelor` user, we should change this `chown`.
COPY --chown=opam . .

################################################################################
##   ___
##  |   \ ___ _ __ ___
##  | |) / -_) '_ (_-<
##  |___/\___| .__/__/
##           |_|
##
##  The `deps` stage only depends on `.opam` files. It uses them to set up all
##  the dependencies, both system and OCaml. Because `.opam` files change more
##  rarely than the rest, this stage should be re-usable almost always. It
##  relies on the OPAM files being up-to-date, which should therefore be tested.

FROM base AS deps

## FIXME: Once there is a `dancelor` user, we should change this `chown`.
COPY --chown=opam *.opam .
RUN opam install . --yes --depext-only
RUN opam install . --yes --deps-only --with-test --with-doc

################################################################################
##   ___      _ _    _
##  | _ )_  _(_) |__| |
##  | _ \ || | | / _` |
##  |___/\_,_|_|_\__,_|
##
##  The `build` stage is where we build the actual tool. This depends on all the
##  files on the outside and will be re-computed with the smallest change. At
##  the end, it contains everything, including a compiled version of Dancelor.

FROM deps AS build

## FIXME: Once there is a `dancelor` user, we should change this `chown`.
COPY --chown=opam . .
RUN opam exec -- make
