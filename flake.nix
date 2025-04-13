{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    ## FIXME: Remove once LilyPond can be taken from NixOS unstable.
    nixpkgs2211.url = "github:nixos/nixpkgs/nixos-22.11";

    flake-parts.url = "github:hercules-ci/flake-parts";
    topiary.url = "github:tweag/topiary";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./nix/application.nix
        ./nix/environment.nix
        ./nix/nixosmodule.nix
        ./nix/nixpkgs.nix
        ./nix/package.nix
      ];

      systems = [ "x86_64-linux" ];

      ## Improve the way `inputs'` are computed by also handling the case of
      ## flakes having a `lib.${system}` attribute.
      ##
      perInput = system: flake: if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };

  nixConfig = {
    extra-trusted-substituters = [
      "https://dancelor.cachix.org/"
      "https://pre-commit-hooks.cachix.org/"
    ];
    extra-trusted-public-keys = [
      "dancelor.cachix.org-1:Q2pAI0MA6jIccQQeT8JEsY+Wfwb/751zmoUHddZmDyY="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
    ];
  };
}
