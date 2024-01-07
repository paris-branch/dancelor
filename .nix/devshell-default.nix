{ ... }: {
  perSystem = { self', pkgs, config, ... }: {
    devShells.default = pkgs.mkShell {
      buildInputs = with self'.packages.dancelor.scope; [
        merlin
        ocaml-lsp-server
        ocp-indent
        utop
        (pkgs.python3.withPackages (p: with p; [ pytest selenium ]))
      ];
      inputsFrom = [ self'.packages.dancelor ];
      shellHook = config.pre-commit.installationScript;
    };
  };
}
