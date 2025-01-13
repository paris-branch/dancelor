## Dancelor as a runnable application. The development environment will need to
## know the runtime inputs as well, so we expose them.

{ self, ... }:

{
  flake.makeRuntimeInputs =
    pkgs: with pkgs; [
      gh
      git
      freepats
      timidity
      lilypond
    ];

  perSystem =
    {
      self',
      pkgs,
      ...
    }:
    {
      apps.default = {
        type = "app";
        program =
          let
            dancelor = pkgs.writeShellApplication {
              name = "dancelor";
              runtimeInputs = self.makeRuntimeInputs pkgs;
              text = ''
                ${self'.packages.default}/bin/dancelor \
                  --share ${self'.packages.default}/share/dancelor \
                  "$@"
              '';
            };
          in
          "${dancelor}/bin/dancelor";
      };
    };
}
