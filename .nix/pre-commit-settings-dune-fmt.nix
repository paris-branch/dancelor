{ ... }: {
  perSystem = { pkgs, ... }: {
    pre-commit.settings.hooks.dune-fmt = {
      enable = true;
      name = "dune-fmt";
      entry = let
        dune-fmt = pkgs.writeShellApplication {
          name = "dune-fmt";
          text = ''
            export PATH=${pkgs.ocaml}/bin:$PATH
            exec ${pkgs.dune_3}/bin/dune fmt "$@"
          '';
        };
      in "${dune-fmt}/bin/dune-fmt";
      pass_filenames = false;
    };
  };
}
