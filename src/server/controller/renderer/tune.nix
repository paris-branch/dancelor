{
  pkgs,
  withArgumentType,
  ...
}:

let
  inherit (builtins)
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
      stylesheet = mkOption {
        description = "A stylesheet to inject into the resulting SVG.";
        type = types.str;
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
    };
  };

  ## TODO: Creating a file from concatenating files can be done directly in Nix,
  ## rather than calling `cat`.
  ## TODO: In fact, we don't need to concatenate files at all, because LilyPond
  ## and Guile both have a mechanism to load other files.
  tuneScheme =
    runCommand "preamble.scm"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
      }
      ''
        {
          cat ${./tune/scheme/extlib.scm}
          cat ${./tune/scheme/extlylib.scm}
          cat ${./tune/scheme/get_partial.scm}
          cat ${./tune/scheme/duration_of_music.scm}
          cat ${./tune/scheme/skip_as_repeat.scm}
          cat ${./tune/scheme/scottish_chords/jig_chords.scm}
          cat ${./tune/scheme/scottish_chords/reel_chords.scm}
          cat ${./tune/scheme/scottish_chords/waltz_chords.scm}
          cat ${./tune/scheme/scottish_chords/chords.scm}
          cat ${./tune/scheme/fancy_unfold_repeats/unfold_first_volta_repeat.scm}
          cat ${./tune/scheme/fancy_unfold_repeats/extract_span.scm}
          cat ${./tune/scheme/fancy_unfold_repeats/split_rhythmic_event_at.scm}
          cat ${./tune/scheme/fancy_unfold_repeats/add_trailing_silence.scm}
          cat ${./tune/scheme/fancy_unfold_repeats/fancy_unfold_repeats.scm}
          cat ${./tune/scheme/bar_numbering/repeat_aware.scm}
        } > $out
      '';

  ## TODO: cf tuneScheme
  ##
  ## NOTE: We could put the SVG book in second place, and avoid the argument
  ## `--define-default=backend=svg`, but somehow this messes up the integration
  ## of the Elementaler font into the resulting SVGs. The SVG-first approach
  ## does not, however, seem to bother the PS/PDF generation.
  makeTuneLilypond =
    {
      slug,
      content,
      first_bar,
      tempo_unit,
      tempo_value,
      chords_kind,
      show_bar_numbers,
      ...
    }:
    let
      contentFile = writeText "tune-${slug}-content.ly" content;
    in
    runCommand "tune-${slug}.ly"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
      }
      ''
        {
          cat ${./tune/lilypond/preamble.ly}
          printf '#(load "%s")\n' ${tuneScheme}
          cat ${./tune/lilypond/macros.ly}
          if ${if show_bar_numbers then "true" else "false"}; then
            cat ${./tune/lilypond/bar_numbering/repeat_aware.ly}
            cat ${./tune/lilypond/bar_numbering/bar_number_in_instrument_name_engraver.ly}
            cat ${./tune/lilypond/bar_numbering/beginning_of_line.ly}
          else
            printf '\\layout {\\context {\\Score\\omit BarNumber}}\n'
          fi
          cat ${./tune/lilypond/scottish_chords.ly}
          cat ${./tune/lilypond/fancy_unfold_repeats.ly}
          ## SVG
          printf '\\book {\n'
          printf '  \\bookOutputSuffix "svg"\n'
          printf '  \\score {\n'
          printf '    \\layout { \\context { \\Score currentBarNumber = #%d } }\n' ${toString first_bar}
          printf '    { %s }\n' "$(cat "${contentFile}")"
          printf '  }\\markup\\null\n'
          printf '}\n\n'
          ## PDF
          printf "#(ly:set-option 'backend 'ps)\n"
          printf '\\book {\n'
          printf '  \\bookOutputSuffix "pdf"\n'
          printf '  \\score {\n'
          printf '    \\layout { \\context { \\Score currentBarNumber = #%d } }\n' ${toString first_bar}
          printf '    { %s }\n' "$(cat "${contentFile}")"
          printf '  }\\markup\\null\n'
          printf '}\n\n'
          ## OGG
          printf '#(set! make-music the-make-music)\n'
          printf '\\book {\n'
          printf '  \\bookOutputSuffix "ogg"\n'
          printf '  \\score {\n'
          printf '    \\midi { \\tempo ${tempo_unit} = ${toString tempo_value} }\n'
          printf '    \\${chords_kind}Chords \\fancyUnfoldRepeats {\n'
          printf '      %s\n' "$(cat "${contentFile}")"
          printf '    }\n'
          printf '  }\n'
          printf '}\n'
        } > $out
      '';

  makeTuneSnippets = withArgumentType "makeTuneSnippets" tuneType (
    tune@{ slug, stylesheet, ... }:
    runCommand "tune-${slug}-snippets"
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
        buildInputs = with pkgs; [
          lilypond
          timidity
        ];
        FONTCONFIG_FILE =
          with pkgs;
          makeFontsConf {
            fontDirectories = [ source-sans-pro ];
          };
      }
      ''
        export HOME=$(mktemp -d)
        lilypond \
          --loglevel=WARNING \
          --define-default=no-point-and-click \
          --define-default=backend=svg \
          --output=snippet \
          ${makeTuneLilypond tune}

        ## Using `sed`, add a CSS import directive to the given stylesheet after
        ## the `<style>` block.
        sed -i 's|^\(<style.*\)$|\1\n@import url("${stylesheet}");|' snippet-svg.svg

        ## Use TiMidity++ to compile from LilyPond's MIDI to an ogg file.
        timidity \
          --quiet=7 \
          --output-mode=v \
          snippet-ogg.midi

        mkdir $out
        mv snippet-pdf.pdf $out/snippet.pdf
        mv snippet-svg.svg $out/snippet.svg
        mv snippet-ogg.ogg $out/snippet.ogg
      ''
  );

in
{
  inherit
    tuneType
    makeTuneSnippets
    ;
}
