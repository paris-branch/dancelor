## Everything that has to do with the development environment: pre-commit hooks,
## development Shell, formatter, etc.

{ self, inputs, ... }:

{
  imports = [ inputs.pre-commit-hooks.flakeModule ];

  perSystem =
    {
      self',
      pkgs,
      config,
      ...
    }:
    {
      formatter = pkgs.nixfmt-rfc-style;

      pre-commit.settings.hooks = {
        nixfmt-rfc-style.enable = true;
        deadnix.enable = true;
        prettier = {
          enable = true;
          excludes = [ "^flake\\.lock$" ];
        };
        dune-fmt.enable = true;
        dune-opam-sync.enable = true;
        ocp-indent.enable = true;
        opam-lint.enable = true;
      };

      devShells.default = pkgs.mkShell {
        buildInputs =
          ## Runtime inputs
          (self.makeRuntimeInputs pkgs)
          ## Development environment
          ++ (with pkgs; [ topiary ])
          ++ (with pkgs.ocamlPackages; [
            merlin
            ocaml-lsp # called `ocaml-lsp-server` in opam.
            ocamlformat # otherwise LSP complains
            ocp-indent
            utop
          ])
          ## System testing environment
          ++ (with pkgs; [
            firefox
            geckodriver
            (python3.withPackages (
              p: with p; [
                pytest
                pytest-xdist
                selenium
              ]
            ))
          ]);
        inputsFrom = [ self'.packages.default ];
        shellHook = config.pre-commit.installationScript;
      };
    };
}
