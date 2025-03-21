## Everything that has to do with the development environment: pre-commit hooks,
## development Shell, formatter, etc.

{ self, inputs, ... }:

{
  imports = [ inputs.pre-commit-hooks.flakeModule ];

  perSystem =
    {
      self',
      inputs',
      pkgs,
      config,
      ...
    }:

    let
      inherit (pkgs.callPackage "${inputs.topiary}/prefetchLanguages.nix" { })
        prefetchLanguages
        fromNickelFile
        toNickelFile
        ;

      defaultTopiaryConfig = fromNickelFile "${inputs.topiary}/languages.ncl";

      topiaryWrappedWithPrefetchedConfig =
        topiaryConfig:
        ## FIXME: We use Topiary from nixpkgs because the version of the main
        ## repository has some bugs on OCaml formatting at this point. We should
        ## just track the Topiary repository and stay on commits that do not
        ## fail.
        pkgs.writeShellApplication {
          name = "topiary";
          text = ''
            exec ${inputs'.packages.topiary-cli}/bin/topiary \
                -C ${toNickelFile "languages-prefetched.ncl" (prefetchLanguages topiaryConfig)} \
                "$@"
          '';
        };

      ## Keep only OCaml as a language for Topiary.
      myTopiaryConfig = defaultTopiaryConfig // {
        languages = { inherit (defaultTopiaryConfig.languages) ocaml ocaml_interface; };
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
        dune-opam-sync.enable = true;
        ocp-indent.enable = true;
        opam-lint.enable = true;
      };

      devShells.default = pkgs.mkShell {
        buildInputs =
          ## Runtime inputs
          (self.makeRuntimeInputs pkgs)
          ## Development environment
          ++ [ (topiaryWrappedWithPrefetchedConfig myTopiaryConfig) ]
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
