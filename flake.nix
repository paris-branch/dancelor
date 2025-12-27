{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    topiary.url = "github:tweag/topiary";

    camelotte = {
      ## NOTE: This is `github:lesboloss-es/camelotte`, but the `github:` scheme
      ## downloads the archive and therefore does not support the `submodules`
      ## attribute, which Camelotte uses.
      url = "git+https://github.com/lesboloss-es/camelotte.git";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        topiary.follows = "topiary";
        git-hooks.follows = "git-hooks-nix";
        flake-parts.follows = "flake-parts";
      };
    };

    git-hooks-nix = {
      url = "github:cachix/git-hooks.nix";
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
        ./nix/package.nix
        ./nix/tests.nix
        ./src/server/controller/renderer/flake-part.nix
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      ## Improve the way `inputs'` are computed by also handling the case of
      ## flakes having a `lib.${system}` attribute.
      ##
      perInput = system: flake: if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };

      ## Create our `pkgs` set by importing nixpkgs with some overlays. This is
      ## used to inject all of Camelotte, but also sometimes to modify packages
      ## that we depend on. This actually ensures that Camelotte packages will
      ## also pick up those modifications.
      ##
      perSystem =
        { system, ... }:
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.camelotte.overlays.default
            ];
          };
        };
    };

  nixConfig = {
    extra-trusted-substituters = [
      "https://dancelor.cachix.org/"
      "https://pre-commit-hooks.cachix.org/"
      "https://tweag-topiary.cachix.org/"
    ];
    extra-trusted-public-keys = [
      "dancelor.cachix.org-1:Q2pAI0MA6jIccQQeT8JEsY+Wfwb/751zmoUHddZmDyY="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "tweag-topiary.cachix.org-1:8TKqya43LAfj4qNHnljLpuBnxAY/YwEBfzo3kzXxNY0="
    ];
  };
}
