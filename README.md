# Dancelor

A chancelor for Scottish country dance musicians.

## Setting up a development environment

The OCaml dependencies are described in `dune-project`; as far as they are
concerned, this is the single source of truth. The system dependencies are
described in `dancelor.opam.template`. Both of these can be acquired
automatically via OPAM or Nix:

- OPAM can install automatically all the system and OCaml dependencies that are
  necessary to develop Dancelor with:

  ```console
  $ opam install . --depext-only
  $ opam install . --deps-only --with-doc --with-test
  ```

- Nix can provide a development environment with all the necessary dependencies
  as well. Use:

  ```console
  $ nix develop
  ```

  If you are a user of [direnv], you may also want to have the following
  `.envrc` file:

  ```console
  watch_dir .nix
  use flake
  ```

[direnv]: https://direnv.net/

## FAQ

### The fonts in the PDF seem weird

Dancelor uses the _Trebuchet MS_ font in PDF files. Make sure it is installed on
the machine on which Dancelor runs. This usually goes by copying the fonts from
the `assets` directory to `~/.local/share/fonts`.
