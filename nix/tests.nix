{ self, ... }:

{
  imports = [
    ../src/server/controller/renderer/tests.nix
  ];

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
