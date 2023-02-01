{
  inputs = {
    opam-nix.url = github:tweag/opam-nix;
    nixpkgs.follows = "opam-nix/nixpkgs";

    flake-utils.url = github:numtide/flake-utils;
    timidity.url = github:niols/nixpkg-timidity;
  };

  outputs = { self, nixpkgs, opam-nix, flake-utils, timidity }:
    flake-utils.lib.eachDefaultSystem (system:

      ## Curate our own set of packages that will be basically opam-nix's
      ## nixpkgs with one modification: We overwrite the package `timidity` by a
      ## custom version coming from our custom github:niols/nixpkg-timidity
      ## flake that provides a version of TiMidity++ with Ogg Vorbis support.
      ##
      let timidityOverlay = self: super: {
            timidity = timidity.packages.${system}.timidityWithVorbis;
          };
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ timidityOverlay ];
          };

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
          formatter = pkgs.nixfmt;

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
