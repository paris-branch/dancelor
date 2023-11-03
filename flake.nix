{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };

    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.opam-repository.follows = "opam-repository";
    };

    timidity = {
      url = "github:niols/nixpkg-timidity";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks.flakeModule
        ./.nix/devshell-default.nix
        ./.nix/formatter.nix
        ./.nix/app-dancelor.nix
        ./.nix/app-default.nix
        ./.nix/package-dancelor.nix
        ./.nix/package-default.nix
        ./.nix/perinput-lib.nix
        ./.nix/pre-commit-settings.nix
        ./.nix/pre-commit-settings-opam-lint.nix
        ./.nix/systems.nix
        ./.nix/timidity-overlay.nix
      ];
    };

  nixConfig = {
    extra-trusted-substituters =
      [ "https://dancelor.cachix.org/" "https://pre-commit-hooks.cachix.org/" ];
    extra-trusted-public-keys = [
      "dancelor.cachix.org-1:Q2pAI0MA6jIccQQeT8JEsY+Wfwb/751zmoUHddZmDyY="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
    ];
  };
}
