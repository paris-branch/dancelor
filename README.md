# Dancelor

A chancelor for Scottish country dance musicians.

[API documentation](https://paris-branch.github.io/dancelor/dancelor)

## Contributing

### Setting up a development environment

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

### Invariants enforced in this repository

In general, in this repository, we enforce the following invariants:

- OCaml files should be indented with [`ocp-indent`]. The project and its
  documentation should build fine, and the tests should run correctly as well.
  Basically, always check `make && make doc && make test`.

- OPAM files should be valid, that is they should pass the `opam lint` check
  without raising any warnings or errors.

- OPAM files should be in sync with the `dune-project` file. In case of lack of
  synchrony, `dune-project` holds the truth.

- OPAM files should contain enough information to spin up an environment able to
  build Dancelor.

- Dune files should be formatted with `dune fmt`.

- Nix code should be formatted with [nixfmt] and should not include any unused
  variable or piece of code.

- Nix files should contain enough information to spin up an environment able to
  build Dancelor.

- HTML, CSS and YAML files should be formatted with [Prettier].

[nixfmt]: https://github.com/serokell/nixfmt
[prettier]: https://prettier.io/
[`ocp-indent`]: http://www.typerex.org/ocp-indent.html

All of these are enforced in CI. For Nix users, the formatting and synchrony of
the files will be checked at pre-commit time in the development environment;
this avoids pushing invalid commits and stressing the CI for nothing.

## FAQ

### The fonts in the PDF seem weird

Dancelor uses the _Trebuchet MS_ font in PDF files. Make sure it is installed on
the machine on which Dancelor runs. This usually goes by copying the fonts from
the `assets` directory to `~/.local/share/fonts`.
