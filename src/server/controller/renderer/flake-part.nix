{ inputs, ... }:

let
  testTune = {
    slug = "test-slug";
    name = "Test Name";
    composer = "Test Composer";
    content = ''
      \version "2.18.2"
      <<
        \new Voice {
          \key f \minor
          \time 3/4
          \repeat volta 2 {
            f'2. g' aes' bes'
          }
        }
        \new ChordNames {
          \chordmode {
            f2. g:m7 aes bes:dim
          }
        }
      >>
    '';
    first_bar = 1;
    tempo_unit = "4";
    tempo_value = 108;
    chords_kind = "reel";
    show_bar_numbers = true;
    show_time_signatures = true;
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
    };
  };

in
{
  perSystem =
    { pkgs, system, ... }:
    let
      inherit
        (import ./renderer.nix {
          inherit (inputs) nixpkgs;
          inherit system;
        })
        makeTuneSnippets
        makeBookPdf
        ;
      testTuneSnippets = makeTuneSnippets testTune;
      testBookPdf = makeBookPdf testBookPdfArg;
    in
    {
      packages.rendererRuntime = pkgs.symlinkJoin {
        name = "renderer-runtime";
        paths = testTuneSnippets.buildInputs ++ testBookPdf.buildInputs;
      };

      checks.renderer-tune-snippets = testTuneSnippets;
      checks.renderer-book-pdf = testBookPdf;
    };
}
