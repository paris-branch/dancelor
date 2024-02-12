{ ... }: {
  perSystem = { self', pkgs, pkgs2211, ... }: {
    apps.dancelor = {
      type = "app";
      program = let
        dancelor = pkgs.writeShellApplication {
          name = "dancelor";
          runtimeInputs = with pkgs2211; [
            git
            timidity
            freepats
            lilypond
            sassc
          ];
          text = ''
            ${self'.packages.dancelor}/bin/dancelor \
              --share ${self'.packages.dancelor}/share/dancelor \
              "$@"
          '';
        };
      in "${dancelor}/bin/dancelor";
    };
  };
}
