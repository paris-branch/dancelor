{
  pkgs,
  withArgumentType,
  setupFontconfigCache,
  ...
}:

let
  inherit (builtins)
    readFile
    toString
    ;

  inherit (pkgs)
    lib
    runCommand
    writeText
    ;

  inherit (lib)
    types
    mkOption
    ;

  tuneType = types.submodule {
    options = {
      slug = mkOption {
        description = "A slug for the tune; it is used to make logs clearer.";
        type = types.str;
      };
      name = mkOption {
        description = "The name of the tune.";
        type = types.str;
      };
      instructions = mkOption {
        description = "Instructions on how to play the tune.";
        type = types.str;
      };
      composer = mkOption {
        description = "The composer of the tune.";
        type = types.str;
      };
      content = mkOption {
        description = "LilyPond code corresponding to the content of the tune.";
        type = types.str;
      };
      first_bar = mkOption {
        description = "What the first bar of the tune should be.";
        type = types.int;
      };
      tempo_unit = mkOption {
        description = ''
          The unit of tempo; eg. `4.`.

          This is used in the ogg generation. In conjunction with `tempo_value`,
          it controls how fast the generated file should play the tune.
        '';
        type = types.str;
      };
      tempo_value = mkOption {
        description = ''
          The value of tempo; eg. `108`.

          This is used in the ogg generation. In conjunction with `tempo_unit`,
          it controls how fast the generated file should play the tune.
        '';
        type = types.int;
      };
      chords_kind = mkOption {
        description = ''
          The kind of chords; eg. `reel`.

          The ogg generation produces a somewhat realistic “vamping” left hand,
          but needs to know which kind of chords are expected, as, for instance,
          jigs and reels will not have the same chords.
        '';
        type = types.str; # FIXME: enum
      };
      show_bar_numbers = mkOption {
        description = ''
          Whether to show the bar numbers.
        '';
        type = types.bool;
      };
      show_time_signatures = mkOption {
        description = ''
          Whether to show time signatures. This will hide all time signatures
          and should therefore be avoided on scores where time signature changes.
        '';
      };
    };
  };

  ## TODO: Use Guile's load instead of catenating files?
  ##
  tuneScheme = writeText "preamble.scm" ''
    ${readFile ./tune/scheme/extlib.scm}
    ${readFile ./tune/scheme/extlylib.scm}
    ${readFile ./tune/scheme/get_partial.scm}
    ${readFile ./tune/scheme/duration_of_music.scm}
    ${readFile ./tune/scheme/skip_as_repeat.scm}
    ${readFile ./tune/scheme/scottish_chords/jig_chords.scm}
    ${readFile ./tune/scheme/scottish_chords/reel_chords.scm}
    ${readFile ./tune/scheme/scottish_chords/waltz_chords.scm}
    ${readFile ./tune/scheme/scottish_chords/chords.scm}
    ${readFile ./tune/scheme/bar_numbering/repeat_aware.scm}
  '';

  makeTuneLilypond =
    {
      slug,
      content,
      first_bar,
      tempo_unit,
      tempo_value,
      chords_kind,
      show_bar_numbers,
      show_time_signatures,
      ...
    }:
    writeText "tune-${slug}.ly" ''
      ${readFile ./tune/lilypond/preamble.ly}
      #(load "${tuneScheme}")
      ${readFile ./tune/lilypond/macros.ly}
      ${
        if show_bar_numbers then
          ''
            ${readFile ./tune/lilypond/bar_numbering/repeat_aware.ly}
            ${readFile ./tune/lilypond/bar_numbering/bar_number_in_instrument_name_engraver.ly}
            ${readFile ./tune/lilypond/bar_numbering/beginning_of_line.ly}
          ''
        else
          ''
            \layout {
              \context {
                \Score
                \omit BarNumber
              }
            }
          ''
      }
      ${
        if show_time_signatures then
          ''
            \numericTimeSignature
          ''
        else
          ''
            \layout {
              \context {
                \Staff
                \remove Time_signature_engraver
              }
            }
          ''
      }
      ${readFile ./tune/lilypond/scottish_chords.ly}
      \score {
        \layout {
          \context {
            \Score
            currentBarNumber = #${toString first_bar}
          }
        }
        { ${content} }
      }
      \markup\null
      #(set! make-music the-make-music)
      \score {
        \midi {
          \tempo ${tempo_unit} = ${toString tempo_value}
        }
        \${chords_kind}Chords \unfoldRepeats {
          ${content}
        }
      }
    '';

  makeTuneSnippets = withArgumentType "makeTuneSnippets" tuneType (
    tune@{ slug, ... }:
    runCommand "tune-${slug}-snippets"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
        buildInputs = with pkgs; [
          lilypond-unstable # for Cairo support as of 27 Nov 2025; can go back to stable in a while
          timidity
        ];
        FONTCONFIG_FILE =
          with pkgs;
          makeFontsConf {
            fontDirectories = [ source-sans-pro ];
          };
      }
      ''
        ${setupFontconfigCache}

        lilypond \
          --loglevel=WARNING \
          --define-default=no-point-and-click \
          --define-default=backend=cairo \
          --formats=pdf,svg \
          --output=snippet \
          ${makeTuneLilypond tune}

        ## Use TiMidity++ to compile from LilyPond's MIDI to an ogg file.
        timidity \
          --quiet=7 \
          --output-mode=v \
          snippet.midi

        mkdir $out && mv snippet.{pdf,svg,ogg} $out/
      ''
  );

in
{
  inherit
    tuneType
    makeTuneSnippets
    ;
}
