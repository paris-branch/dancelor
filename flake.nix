{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    nixpkgs.follows = "opam-nix/nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
    timidity.url = "github:niols/nixpkg-timidity";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks.flakeModule
        ./.nix/systems.nix
        ./.nix/per-input-lib.nix
        ./.nix/timidity-overlay.nix
        ./.nix/pre-commit-settings.nix
        ./.nix/formatter.nix
        ./.nix/package-dancelor.nix
        ./.nix/package-default.nix
        ./.nix/devshell-default.nix
      ];
    };
}
