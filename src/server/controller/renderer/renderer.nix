{
  nixpkgs ? <nixpkgs>,
  system ? builtins.currentSystem,
  ...
}:

let
  pkgs = (import nixpkgs { inherit system; }).appendOverlays [
    (_final: prev: {
      timidity = prev.timidity.override { enableVorbis = true; };
    })
  ];

  inherit (import ./utils.nix { })
    withArgumentType
    ;

  inherit
    (import ./tune.nix {
      inherit
        pkgs
        withArgumentType
        ;
    })
    tuneType
    makeTuneSnippets
    ;

  inherit
    (import ./book.nix {
      inherit
        pkgs
        tuneType
        makeTuneSnippets
        withArgumentType
        ;
    })
    makeBookPdf
    ;

in
{
  inherit
    makeTuneSnippets
    makeBookPdf
    ;
}
