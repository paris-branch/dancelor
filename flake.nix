{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          on = opam-nix.lib.${system};
          scope = on.buildOpamProject { } "dancelor" ./. {
            merlin = "*";
            ocaml-base-compiler = "*";
            ocp-indent = "*";
            utop = "*";
          };
      in
      {
        legacyPackages =
          let overlay = self: super: {
                dancelor = super.dancelor.overrideAttrs (oa: {
                  nativeBuildInputs = oa.nativeBuildInputs or [ ] ++ [ pkgs.makeWrapper ];
                  postInstall =
                    "wrapProgram $out/bin/dancelor-server --prefix PATH : ${
                      pkgs.lib.makeBinPath [
                        pkgs.freepats
                        pkgs.inkscape
                        pkgs.lilypond
                        pkgs.timidity
                      ]
                    }";
                });
              };
          in
          scope.overrideScope' overlay;

      defaultPackage = self.legacyPackages.${system}.dancelor;

      devShells.default = pkgs.mkShell {
          buildInputs = [ scope.merlin scope.ocp-indent scope.utop ];
          inputsFrom = [ scope.dancelor ];
      };
    });
}
