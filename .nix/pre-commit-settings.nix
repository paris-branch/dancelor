{ ... }: {
  perSystem = { ... }: {
    pre-commit.settings.hooks = {
      nixfmt.enable = true;
      deadnix.enable = true;
      prettier.enable = true;
    };
  };
}
