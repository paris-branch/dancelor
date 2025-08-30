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
