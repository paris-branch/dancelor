{ inputs, ... }: {
  perSystem = { inputs', self', system, pkgs, config, ... }: {
    ## Curate our own set of packages that will be basically opam-nix's
    ## nixpkgs with one modification: We overwrite the package `timidity` by
    ## a custom version coming from our custom github:niols/nixpkg-timidity
    ## flake that provides a version of TiMidity++ with Ogg Vorbis support.
    ##
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (self: super: {
          timidity = inputs'.timidity.packages.timidityWithVorbis;
        })
      ];
    };
  };
}
