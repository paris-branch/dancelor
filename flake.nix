{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    ## FIXME: Remove once LilyPond can be taken from NixOS unstable.
    nixpkgs2211.url = "github:nixos/nixpkgs/nixos-22.11";

    flake-parts.url = "github:hercules-ci/flake-parts";

    timidity = {
      url = "github:niols/nixpkg-timidity";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks.flakeModule
        ./.nix/app-devshell.nix
        ./.nix/nixosmodule.nix
        ./.nix/package.nix
      ];

      systems = [ "x86_64-linux" ];

      ## Overwrite the package `timidity` by a custom version coming from our
      ## custom `github:niols/nixpkg-timidity` flake that provides a version of
      ## TiMidity++ with Ogg Vorbis support.
      ##
      flake.overlays.timidityWithVorbis = _final: prev: {
        timidity = inputs.timidity.packages.${prev.stdenv.hostPlatform.system}.timidityWithVorbis;
      };

      perSystem =
        { system, pkgs, ... }:
        {
          formatter = pkgs.nixfmt-rfc-style;

          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ self.overlays.timidityWithVorbis ];
          };
          _module.args.pkgs2211 = import inputs.nixpkgs2211 {
            inherit system;
            overlays = [ self.overlays.timidityWithVorbis ];
          };

          pre-commit.settings.hooks = {
            nixfmt-rfc-style.enable = true;
            deadnix.enable = true;
            prettier.enable = true;
            dune-fmt.enable = true;
            dune-opam-sync.enable = true;
            opam-lint.enable = true;
          };
        };

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
