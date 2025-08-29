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
      lib,
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
      formatter = pkgs.nixfmt-rfc-style;

      pre-commit.settings.hooks = {
        nixfmt-rfc-style.enable = true;
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
          ## Development environment
          ++ [ (gitHookBinFor myTopiaryConfig) ]
          ++ (with pkgs.ocamlPackages; [
            merlin
            ocaml-lsp # called `ocaml-lsp-server` in opam.
            ocamlformat # otherwise LSP complains
            ocp-indent
            utop
          ])
          ## System testing environment
          ++ (self.makeTestInputs pkgs);
        inputsFrom = [ self'.packages.default ];
        shellHook = config.pre-commit.installationScript;
        ## Dancelor runs Nix, which needs to grab `nixpkgs` and `nixpkgs2211`.
        ## We expose the flake inputs under those names.
        NIX_PATH = "nixpkgs=${inputs.nixpkgs}:nixpkgs2211=${inputs.nixpkgs2211}";
      };
    };
}
