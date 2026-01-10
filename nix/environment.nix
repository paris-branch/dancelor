## Everything that has to do with the development environment: pre-commit hooks,
## development Shell, formatter, etc.

{ self, inputs, ... }:

{
  imports = [ inputs.git-hooks-nix.flakeModule ];

  perSystem =
    {
      self',
      inputs',
      pkgs,
      config,
      ...
    }:

    let
      inherit (inputs'.topiary.lib) gitHookBinFor gitHookFor;
      myTopiaryConfig = {
        includeLanguages = [
          "ocaml"
          "ocaml_interface"
        ];
      };
    in

    {
      formatter = pkgs.nixfmt;

      pre-commit.settings.hooks = {
        nixfmt.enable = true;
        deadnix.enable = true;
        prettier = {
          enable = true;
          excludes = [ "^flake\\.lock$" ];
        };
        dune-fmt.enable = true;

        topiary-latest = gitHookFor myTopiaryConfig // {
          enable = true;
        };
      };

      devShells.default = pkgs.mkShell {
        buildInputs =
          ## Runtime inputs
          (self.makeRuntimeInputs pkgs)
          ## Test inputs
          ++ (with pkgs.ocamlPackages; [
            alcotest
            ppx_deriving_qcheck
            qcheck
            qcheck-alcotest
          ])
          ## Development environment
          ++ [ (gitHookBinFor myTopiaryConfig) ]
          ++ (with pkgs.ocamlPackages; [
            self'.packages.ocaml-prune
            merlin
            ocaml-lsp # called `ocaml-lsp-server` in opam.
            ocamlformat # otherwise LSP complains
            ocp-indent
            utop
          ])
          ## System testing environment
          ++ (self.makeIntegrationCheckInputs pkgs);
        inputsFrom = [ self'.packages.dancelor ];
        shellHook = config.pre-commit.installationScript;
        ## Dancelor runs Nix, and we want it to use the same `nixpkgs`. We
        ## expose the flake input `nixpkgs` as a channel in the Nix path.
        NIX_PATH = "nixpkgs=${inputs.nixpkgs}";
      };

      packages.ocaml-merlin-lib = pkgs.ocamlPackages.buildDunePackage {
        pname = "merlin-lib";
        version = pkgs.ocamlPackages.merlin.version;
        src = pkgs.ocamlPackages.merlin.src;

        buildInputs = with pkgs.ocamlPackages; [
          csexp
        ];
      };

      packages.ocaml-index = pkgs.ocamlPackages.buildDunePackage {
        pname = "ocaml-index";
        version = pkgs.ocamlPackages.merlin.version;
        src = pkgs.ocamlPackages.merlin.src;

        buildInputs = with pkgs.ocamlPackages; [
          self'.packages.ocaml-merlin-lib
          merlin
          csexp
        ];
      };

      packages.ocaml-prune = pkgs.ocamlPackages.buildDunePackage {
        pname = "prune";
        version = "dev";
        src = pkgs.fetchFromGitHub {
          owner = "samoht";
          repo = "prune";
          rev = "74a572b4acbeff91e613ddbe90b634211f3f20f8";
          sha256 = "sha256-bg8hu8W5PBRQnCbW+eUdTNkQ5Y1n3FW1KF1pofEHXDo=";
        };

        propagatedBuildInputs = with pkgs.ocamlPackages; [
          self'.packages.ocaml-index
        ];

        buildInputs = with pkgs.ocamlPackages; [
          dune-build-info
          merlin
          yojson
          bos
          cmdliner
          rresult
          logs
          fmt
          logs
          re
          ppxlib
        ];
      };
    };
}
