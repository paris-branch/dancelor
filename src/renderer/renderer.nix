{
  nixpkgs ? <nixpkgs>,
  nixpkgs2211 ? <nixpkgs2211>, # for LilyPond 2.22
  ...
}:

let
  pkgs = (import nixpkgs { }).appendOverlays [
    (_final: prev: {
      lilypond = (import nixpkgs2211 { }).lilypond;
      timidity = prev.timidity.override { enableVorbis = true; };
    })
  ];

  inherit (import ./utils.nix { })
    withArgumentType
    ;

  inherit (import ./tune.nix { inherit pkgs withArgumentType; })
    tuneType
    makeTunePdf
    makeTuneSvg
    makeTuneOgg
    ;

  inherit
    (import ./book.nix {
      inherit
        pkgs
        tuneType
        makeTunePdf
        withArgumentType
        ;
    })
    makeBookPdf
    ;

in
{
  inherit
    tuneType
    makeTunePdf
    makeTuneSvg
    makeTuneOgg
    makeBookPdf
    ;
}
