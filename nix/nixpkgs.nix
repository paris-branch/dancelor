## We expose a variant of nixpkgs with an older LilyPond (gotten from NixOS
## 22.11) and a modified TiMidity++ that is compiled with Vorbis support.

{ inputs, ... }:

{
  perSystem =
    { pkgs, system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          (_final: prev: {
            timidity = prev.timidity.override { enableVorbis = true; };
            lilypond = inputs.nixpkgs2211.legacyPackages.${system}.lilypond;
          })

          (_final: prev: {
            ocamlPackages = prev.ocamlPackages.overrideScope (
              finalScope: _prevScope: {
                argon2 = finalScope.callPackage ./package/ocaml/argon2.nix { };
                ppxlib = finalScope.callPackage ./package/ocaml/ppxlib.nix { };
                ppx_deriving = finalScope.callPackage ./package/ocaml/ppx_deriving.nix { };
                ppx_deriving_qcheck = finalScope.callPackage ./package/ocaml/ppx_deriving_qcheck.nix { };
                ppx_deriving_yojson = finalScope.callPackage ./package/ocaml/ppx_deriving_yojson.nix { };
              }
            );
          })
        ];
      };
    };
}
