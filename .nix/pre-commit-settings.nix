{ ... }: {
  perSystem = { ... }: {
    pre-commit.settings.hooks = {
      nixfmt.enable = true;
      deadnix.enable = true;
      prettier.enable = true;
      dune-fmt.enable = true;
      dune-opam-sync.enable = true;
      ocp-indent.enable = true;
    };
  };
}
