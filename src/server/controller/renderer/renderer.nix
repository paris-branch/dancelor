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

  inherit (import ./utils.nix { inherit pkgs; })
    withArgumentType
    setupFontconfigCache
    setupLuaotfloadCache
    myTexlive
    myFontconfigFile
    ;

  inherit
    (import ./tune.nix {
      inherit
        pkgs
        withArgumentType
        setupFontconfigCache
        myFontconfigFile
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
        setupFontconfigCache
        setupLuaotfloadCache
        myTexlive
        myFontconfigFile
        ;
    })
    makeBookPdf
    makeSetPdf
    makeSetsZip
    ;

in
{
  inherit
    makeTuneSnippets
    makeBookPdf
    makeSetPdf
    makeSetsZip
    ;
}
