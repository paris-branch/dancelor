{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    ## The three following line make this flake follow the official OPAM
    ## repository. This is necessary because `ppx_monad` is not currently in
    ## `opam-nix`'s version of the OPAM repository.
    opam-nix.inputs.opam-repository.follows = "opam-repository";
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, ... }@inputs:
    let package = "dancelor";
    in flake-utils.lib.eachDefaultSystem (system: {
      legacyPackages = let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        scope =
          on.buildOpamProject { } package ./. { ocaml-base-compiler = null; };
        overlay = self: super: {
          # Your overrides go here
          dancelor = super.dancelor.overrideAttrs (oa: {
            nativeBuildInputs = oa.nativeBuildInputs or [ ]
              ++ [ pkgs.makeWrapper ];
            postInstall =
              "wrapProgram $out/bin/dancelor-server --prefix PATH : ${
                pkgs.lib.makeBinPath [
                  pkgs.inkscape
                  pkgs.lilypond
                  pkgs.timidity
                  pkgs.freepats
                ]
              }";
          });
        };
      in scope.overrideScope' overlay;

      defaultPackage = self.legacyPackages.${system}.${package};
    });
}
