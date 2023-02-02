{ ... }: {
  perSystem = { pkgs, ... }: {
    pre-commit.settings.hooks.opam-lint = {
      enable = true;
      entry = "${pkgs.opam}/bin/opam lint";
      files = "\\.opam$";
    };
  };
}
