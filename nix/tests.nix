{ self, ... }:

{
  flake.makeIntegrationCheckInputs =
    pkgs: with pkgs; [
      firefox
      geckodriver
      xclip # for pyperclip
      (python3.withPackages (
        p: with p; [
          pyperclip
          pytest
          pytest-xdist
          pyyaml
          selenium
        ]
      ))
    ];

  perSystem =
    { pkgs, ... }:
    {
      checks.integration = pkgs.testers.runNixOSTest {
        name = "integration";

        imports = [
          ../tests/scripts/nixosTest.nix
          {
            nodes.machine = {
              imports = [ self.nixosModules.dancelor ];
              environment.systemPackages = (self.makeIntegrationCheckInputs pkgs);
            };
          }
        ];
      };
    };
}
