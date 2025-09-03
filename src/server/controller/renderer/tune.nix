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
      name = mkOption {
        description = "The name of the tune.";
        type = types.str;
      };
      composer = mkOption {
        description = "The composer of the tune.";
        type = types.str;
      };
      content = mkOption { type = types.str; };
      first_bar = mkOption {
        description = "What the first bar of the tune should be.";
        type = types.int;
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
        allowSubtitutes = false;
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
        } > $out
      '';

  ## TODO: cf tuneScheme
  makeTuneLilypond =
    {
      slug,
      tune,
      tempo_unit,
      tempo_value,
      chords_kind,
    }:
    let
      contentFile = writeText "tune-${slug}-content.ly" tune.content;
    in
    runCommand "tune-${slug}.ly"
      {
        allowSubtitutes = false;
      }
      ''
        {
          cat ${./tune/lilypond/lyversion.ly}
          printf '#(load "%s")\n' ${tuneScheme}
          cat ${./tune/lilypond/layout.ly}
          cat ${./tune/lilypond/paper.ly}
          cat ${./tune/lilypond/cropped.ly}
          cat ${./tune/lilypond/helpers.ly}
          cat ${./tune/lilypond/repeat_volta_fancy.ly}
          cat ${./tune/lilypond/bar_numbering/repeat_aware.ly}
          cat ${./tune/lilypond/bar_numbering/bar_number_in_instrument_name_engraver.ly}
          cat ${./tune/lilypond/bar_numbering/beginning_of_line.ly}
          cat ${./tune/lilypond/scottish_chords.ly}
          cat ${./tune/lilypond/fancy_unfold_repeats.ly}
          cat ${./tune/lilypond/version/header.ly}
          printf '\\score {\n'
          printf '  \\layout { \\context { \\Score currentBarNumber = #%d } }\n' ${toString tune.first_bar}
          printf '  { %s }\n' "$(cat "${contentFile}")"
          printf '}\n\\markup\\null\n'
          printf '#(set! make-music the-make-music)\n'
          printf '\\score {\n'
          printf '  \\midi { \\tempo ${tempo_unit} = ${toString tempo_value} }\n'
          printf '  \\${chords_kind}Chords \\fancyUnfoldRepeats {\n'
          printf '    %s\n' "$(cat "${contentFile}")"
          printf '  }\n'
          printf '}\n'
        } > $out
      '';

  tunePdfArgType = types.submodule {
    options = {
      slug = mkOption { type = types.str; };
      tune = mkOption { type = tuneType; };
    };
  };

  ## TODO: Merge with `makeTuneSnippets`. Maybe we can even do everything in
  ## just one LilyPond call. Either way, this would allow better caching and
  ## reuse, and a cleaner interface.
  makeTunePdf = withArgumentType "makeTunePdf" tunePdfArgType (
    { slug, tune }:
    runCommand "tune-${slug}-pdf"
      {
        allowSubtitutes = false;
        buildInputs = with pkgs; [ lilypond ];
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
          --output=tune \
          ${makeTuneLilypond { inherit slug tune; }}
        mkdir $out
        mv tune.pdf $out
      ''
  );

  tuneSnippetsArgType = types.submodule {
    options = {
      slug = mkOption { type = types.str; };
      tune = mkOption { type = tuneType; };
      stylesheet = mkOption {
        description = "A stylesheet to inject into the resulting SVG.";
        type = types.str;
      };
      tempo_unit = mkOption { type = types.str; };
      tempo_value = mkOption { type = types.int; };
      chords_kind = mkOption { type = types.str; };
    };
  };

  makeTuneSnippets = withArgumentType "makeTuneSnippets" tuneSnippetsArgType (
    {
      slug,
      tune,
      stylesheet,
      tempo_unit,
      tempo_value,
      chords_kind,
    }:
    runCommand "tune-${slug}-snippets"
      {
        allowSubtitutes = false;
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
          --output=tune \
          ${makeTuneLilypond {
            inherit
              slug
              tune
              tempo_unit
              tempo_value
              chords_kind
              ;
          }}

        ## Using `sed`, add a CSS import directive to the given stylesheet after
        ## the `<style>` block.
        sed -i 's|^\(<style.*\)$|\1\n@import url("${stylesheet}");|' tune.svg

        ## Use TiMidity++ to compile from LilyPond's MIDI to an ogg file.
        timidity \
          --quiet=7 \
          --output-mode=v \
          tune.midi

        mkdir $out
        mv tune.svg tune.ogg $out
      ''
  );

in
{
  inherit
    tuneType
    makeTunePdf
    makeTuneSnippets
    ;
}
