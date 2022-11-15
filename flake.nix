{
  inputs = {
    opam-nix.url = github:tweag/opam-nix;
    nixpkgs.url = github:niols/nixpkgs/enable-vorbis-by-default;

    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, flake-utils, opam-nix, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:

      let pkgs = nixpkgs.legacyPackages.${system};
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
