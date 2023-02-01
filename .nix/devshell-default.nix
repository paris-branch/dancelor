{ ... }: {
  perSystem = { self', pkgs, config, ... }: {
    devShells.default = pkgs.mkShell {
      buildInputs = with self'.packages.dancelor.scope; [
        merlin
        ocaml-lsp-server
        ocp-indent
        utop
      ];
      inputsFrom = [ self'.packages.dancelor ];
      shellHook = config.pre-commit.installationScript;
    };
  };
}
