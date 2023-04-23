{ ... }: {
  perSystem = { pkgs, ... }: {
    pre-commit.settings.hooks.dune-fmt = {
      enable = true;
      name = "dune-fmt";
      entry = "${pkgs.dune_3}/bin/dune fmt";
      pass_filenames = false;
    };
  };
}
