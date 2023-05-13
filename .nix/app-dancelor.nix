{ ... }: {
  perSystem = { self', pkgs, ... }: {
    apps.dancelor = {
      type = "app";
      program = let
        dancelor = pkgs.writeShellApplication {
          name = "dancelor";
          runtimeInputs = with pkgs; [
            timidity
            freepats
            inkscape
            lilypond
            sassc
            xvfb-run
          ];
          text = ''
            ${self'.packages.dancelor}/bin/dancelor-server "$@"
          '';
        };
      in "${dancelor}/bin/dancelor";
    };
  };
}
