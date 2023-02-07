{ ... }: {
  perSystem = { inputs', ... }: {
    pre-commit.settings.hooks = {
      nixfmt.enable = true;
      deadnix.enable = true;
      prettier.enable = true;
      topiary = inputs'.topiary.lib.pre-commit-hook;
    };
  };
}
