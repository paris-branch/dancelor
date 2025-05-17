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

          (final: prev: {
            ocamlPackages = prev.ocamlPackages // {
              argon2 = final.ocamlPackages.callPackage ./package/ocaml/argon2.nix { };
            };
          })
        ];
      };
    };
}
