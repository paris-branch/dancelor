{ inputs, ... }:

let
  testTune = {
    slug = "test-slug";
    name = "Test Name";
    composer = "Test Composer";
    content = "{ a1 b c d }";
    first_bar = 1;
    stylesheet = "<dummy>";
    tempo_unit = "4";
    tempo_value = 108;
    chords_kind = "reel";
  };

  testBookPdfArg = {
    book = {
      slug = "test-slug";
      title = "Test Title";
      editor = "Test Editor";
      contents = [ ];
      simple = false;
    };
    specificity = "none";
    headers = true;
    pdf_metadata = {
      title = "Test PDF Title";
      authors = [
        "Test PDF Author 1"
        "Test PDF Author 2"
      ];
      subjects = [
        "Test PDF Subject 1"
        "Test PDF Subject 2"
      ];
      creator = "Test PDF Creator";
    };
  };

in
{
  perSystem =
    { system, ... }:
    let
      inherit
        (import ./renderer.nix {
          inherit (inputs) nixpkgs nixpkgs2211;
          inherit system;
        })
        makeTuneSnippets
        makeBookPdf
        ;
    in
    {
      checks.renderer-tune = makeTuneSnippets testTune;
      checks.renderer-book = makeBookPdf testBookPdfArg;
    };
}
