{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    nixpkgs.follows = "opam-nix/nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
    timidity.url = "github:niols/nixpkg-timidity";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks.flakeModule
        ./.nix/systems.nix
        ./.nix/per-input-lib.nix
        ./.nix/timidity-overlay.nix
      ];

      perSystem = { inputs', self', system, pkgs, config, ... }: {

        pre-commit.settings.hooks = { nixfmt.enable = true; };

        formatter = pkgs.nixfmt;

        packages =
          (inputs'.opam-nix.lib.buildOpamProject { pkgs = pkgs; } "dancelor"
            ./. {
              merlin = "*";
              ocaml-base-compiler = "*";
              ocaml-lsp-server = "*";
              ocp-indent = "*";
              utop = "*";
            }) // {
              default = self'.packages.dancelor;
            };

        devShells.default = pkgs.mkShell {
          buildInputs = with self'.packages; [
            merlin
            ocaml-lsp-server
            ocp-indent
            utop
          ];
          inputsFrom = [ self'.packages.dancelor ];
          shellHook = config.pre-commit.installationScript;
        };
      };
    };
}
