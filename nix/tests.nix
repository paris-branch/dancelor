{ self, ... }:

{
  flake.makeTestInputs =
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
        imports = [
          ../tests/scripts/nixosTest.nix
          {
            nodes.machine = {
              imports = [ self.nixosModules.dancelor ];
              environment.systemPackages = (self.makeTestInputs pkgs);
            };
          }
        ];
      };
    };
}
