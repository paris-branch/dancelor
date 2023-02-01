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
      systems = [ "x86_64-linux" ];

      ## Improve the way `inputs'` are computed by also handling the case of
      ## flakes having a `lib.${system}` attribute.
      ##
      perInput = system: flake:
        if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };

      imports = [ inputs.pre-commit-hooks.flakeModule ];

      perSystem = { inputs', self', system, pkgs, config, ... }: {
        ## Curate our own set of packages that will be basically opam-nix's
        ## nixpkgs with one modification: We overwrite the package `timidity` by
        ## a custom version coming from our custom github:niols/nixpkg-timidity
        ## flake that provides a version of TiMidity++ with Ogg Vorbis support.
        ##
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              timidity = inputs'.timidity.packages.timidityWithVorbis;
            })
          ];
        };

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
