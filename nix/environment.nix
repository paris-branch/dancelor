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
      lib,
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
        pkgs.writeShellApplication {
          name = "topiary";
          text = ''
            exec ${inputs'.topiary.packages.topiary-cli}/bin/topiary \
                -C ${toNickelFile "languages-prefetched.ncl" (prefetchLanguages topiaryConfig)} \
                "$@"
          '';
        };

      topiaryGitHook = topiaryConfig: {
        name = "topiary";
        entry = "${topiaryWrappedWithPrefetchedConfig topiaryConfig}/bin/topiary format";
        files =
          let
            inherit (lib) concatMap attrValues concatStringsSep;
            extensions = concatMap (c: c.extensions) (attrValues topiaryConfig.languages);
          in
          "\\.(" + concatStringsSep "|" extensions + ")$";
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

        topiary-latest = topiaryGitHook myTopiaryConfig // {
          enable = true;
        };
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
            xclip # for pyperclip
            (python3.withPackages (
              p: with p; [
                pyperclip
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
