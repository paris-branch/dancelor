{
  inputs = {
    opam-nix.url = github:niols/opam-nix/add-xvfb-to-external-dependencies-map;
    nixpkgs.follows = "opam-nix/nixpkgs";

    flake-utils.url = github:numtide/flake-utils;
    timidity.url = github:niols/nixpkg-timidity;
  };

  outputs = { self, nixpkgs, opam-nix, flake-utils, timidity }:
    flake-utils.lib.eachDefaultSystem (system:

      let timidityOverlay = self: super: {
            timidity = timidity.packages.${system}.timidityWithVorbis;
          };
          pkgs = nixpkgs.legacyPackages.${system}.extend timidityOverlay;

          on = opam-nix.lib.${system};

          packages = on.buildOpamProject { pkgs = pkgs; } "dancelor" ./. {
            merlin = "*";
            ocaml-base-compiler = "*";
            ocaml-lsp-server = "*";
            ocp-indent = "*";
            utop = "*";
          };
      in
        {
          packages = packages // {
            default = self.packages.${system}.dancelor;
          };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              packages.merlin
              packages.ocaml-lsp-server
              packages.ocp-indent
              packages.utop
            ];
            inputsFrom = [ packages.dancelor ];
          };
        });
}
