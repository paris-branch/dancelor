## Dancelor as a runnable application. The development environment will need to
## know the runtime inputs as well, so we expose them.

{ self, inputs, ... }:

{
  flake.makeRuntimeInputs =
    pkgs: with pkgs; [
      busybox # provides `sed`
      gh
      git
      nix # need the Nix command

      ## The renderer requires some packages at runtime (eg. LilyPond and a
      ## variant of TiMidity++), which this derivation provides. They would be
      ## acquired dynamically by Nix, but we would rather avoid that extra
      ## computation time, so we add them here.
      self.packages.${pkgs.system}.rendererRuntime
    ];

  perSystem =
    {
      self',
      pkgs,
      ...
    }:
    {
      apps.dancelor = {
        type = "app";
        program =
          let
            dancelor = pkgs.writeShellApplication {
              name = "dancelor";
              runtimeInputs = self.makeRuntimeInputs pkgs;
              text = ''
                ${self'.packages.dancelor}/bin/dancelor \
                  --share ${self'.packages.dancelor}/share/dancelor \
                  --nixpkgs ${inputs.nixpkgs} \
                  --nixpkgs2211 ${inputs.nixpkgs2211} \
                  "$@"
              '';
            };
          in
          "${dancelor}/bin/dancelor";
      };
    };
}
