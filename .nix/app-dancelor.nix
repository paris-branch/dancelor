{ ... }: {
  perSystem = { self', pkgs, ... }: {
    apps.dancelor = {
      type = "app";
      program = let
        dancelor = pkgs.writeShellApplication {
          name = "dancelor";
          runtimeInputs = with pkgs; [ timidity freepats lilypond sassc ];
          text = ''
            ${self'.packages.dancelor}/bin/dancelor-server "$@"
          '';
        };
      in "${dancelor}/bin/dancelor";
    };
  };
}
