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
        ./.nix/pre-commit-settings-dune-opam-sync.nix
        ./.nix/pre-commit-settings-dune-fmt.nix
        ./.nix/pre-commit-settings-ocp-indent.nix
        ./.nix/pre-commit-settings-opam-lint.nix
        ./.nix/systems.nix
        ./.nix/timidity-overlay.nix
      ];
    };
}
