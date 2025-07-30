# <img alt="Dancelor" src="src/static/logo.svg" height="100px">

A chancelor for Scottish country dance musicians.

[API documentation](https://paris-branch.github.io/dancelor/dancelor)

## Contributing

### Setting up a development environment

The OCaml dependencies are described in `nix/package.nix`; these files should
not go out of sync and the CI checks that Dancelor builds fine in this Nix-based
environments. The system dependencies are described in `nix/application.nix`.

Nix can provide an environment with all the necessary dependencies, both OCaml
and system, as well as development tools:

```console
$ nix develop
```

If you are a user of [direnv], you may also want to have the following `.envrc`
file:

```
watch_dir nix/
use flake
```

[direnv]: https://direnv.net/

### Running and writing tests

Dancelor contains two kinds of tests: unit tests and system tests. Unit tests
are fully integrated with Dune so nothing much is required there. There is a
`make unit-tests` target, but it is really just an alias for `dune test`. Unit
tests are written either right after the function that they test, using
[ppx_inline_test], or in `tests/unit`, using Alcotest. The latter usually
contain QCheck-based tests.

[ppx_inline_test]: https://github.com/janestreet/ppx_inline_test

System tests are heavier tests on the final product. They run using [Selenium]'s
Python API and Firefox driver. To run them, you need Python 3, Selenium and
`pytest` on your machine. The Nix environment provides them; otherwise, you can
install them with pip:

```console
$ pip install pytest selenium
```

You also need the Firefox web browser. It is not provided by the Nix environment
but you can easily get it with your machine's package manager. You can now run
the system tests with:

```console
$ pytest
```

This requires having a properly-configured Dancelor instance running in the
background. This instance must listen on port 8080 and use database
`tests/database/`. For running tests, the target `make system-tests` builds
Dancelor if needed, then starts it in the background with the right options, run
the system tests against it and then kills Dancelor again. For developping
tests, the target `make dev-test` launches Dancelor in such a mode.

Writing Selenium scripts can be done manually by mimmicking the ones already
present in `tests/scripts`, in which case you might be interesting in
[Selenium's API documentation][selenium-api-doc], but it is also possible to
rely on the Selenium IDE and exporting tests in `pytest` style.

[selenium]: https://www.selenium.dev/
[selenium-api-doc]: https://www.selenium.dev/selenium/docs/api/py/py-modindex.html

### Invariants enforced in this repository

In general, in this repository, we enforce the following invariants:

- OCaml files should be formatted with [Topiary]. The project and its
  documentation should build fine, and the tests should run correctly as well.
  Basically, always check `make && make doc && make unit-test && make system-test`.

- Dune files should be formatted with `dune fmt`.

- Nix code should be formatted with [`nixfmt-rfc-style`] and should not include
  any unused variable or piece of code.

- Nix files should contain enough information to spin up an environment able to
  build Dancelor.

- HTML, CSS and YAML files should be formatted with [Prettier].

[`nixfmt-rfc-style`]: https://github.com/NixOS/nixfmt
[Prettier]: https://prettier.io/
[Topiary]: https://topiary.tweag.io/

All of these are enforced in CI. For Nix users, the formatting and synchrony of
the files will be checked at pre-commit time in the development environment;
this avoids pushing invalid commits and stressing the CI for nothing.

## FAQ

### The fonts in the PDF seem weird

Dancelor uses the _Trebuchet MS_ font in PDF files. Make sure it is installed on
the machine on which Dancelor runs. This usually goes by copying the fonts from
the `assets` directory to `~/.local/share/fonts`.
