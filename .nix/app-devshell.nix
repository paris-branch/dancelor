{
  perSystem = { self', pkgs, pkgs2211, config, ... }:
    let
      runtimeInputs = (with pkgs; [ git freepats timidity ])
        ++ [ pkgs2211.lilypond ];

    in {
      devShells.default = pkgs.mkShell {
        buildInputs =
          ## Runtime inputs
          runtimeInputs
          ## Development environment
          ++ (with pkgs.ocamlPackages; [
            merlin
            ocaml-lsp # called `ocaml-lsp-server` in opam.
            ocp-indent
            utop
          ])
          ## System testing environment
          ++ (with pkgs; [
            firefox
            geckodriver
            (python3.withPackages (p: with p; [ pytest pytest-xdist selenium ]))
          ]);
        inputsFrom = [ self'.packages.default ];
        shellHook = config.pre-commit.installationScript;
      };

      apps.default = {
        type = "app";
        program = let
          dancelor = pkgs.writeShellApplication {
            name = "dancelor";
            inherit runtimeInputs;
            text = ''
              ${self'.packages.default}/bin/dancelor \
                --share ${self'.packages.default}/share/dancelor \
                "$@"
            '';
          };
        in "${dancelor}/bin/dancelor";
      };
    };
}
