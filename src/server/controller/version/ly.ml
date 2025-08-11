open NesUnix
open Common

module Log = (val Logger.create "controller.version.ly": Logs.LOG)

let get env id =
  let%lwt version = Model.Version.get id in
  Permission.assert_can_get env version;%lwt
  lwt @@ Model.Version.content' version

let prepare_file parameters ?(show_meta = false) ?(meta_in_title = false) ~fname version =
  Log.debug (fun m -> m "Preparing Lilypond file");
  let fname_scm = Filename.chop_extension fname ^ ".scm" in
  let%lwt tune = Model.Version.tune' version in
  let key = Model.Version.key' version in
  let name = Model.VersionParameters.display_name' ~default: (Model.Tune.one_name' tune) parameters in
  let%lwt composer = (String.concat ", " ~last: " and " % List.map Model.Person.name') <$> Model.Tune.composers' tune in
  let composer = Model.VersionParameters.display_composer' ~default: composer parameters in
  let title, piece =
    if show_meta then
      if meta_in_title then name, " " else "", name
    else
      "", ""
  in
  let subtitle, opus =
    if show_meta then
      if meta_in_title then composer, " " else "", composer
    else
      "", ""
  in
  let kind = Model.Tune.kind' tune in
  let (tempo_unit, tempo_value) = Kind.Base.tempo kind in
  Log.debug (fun m -> m "Getting content");
  let content = Model.Version.content' version in

  (* Handle parameters *)
  let content =
    match Model.VersionParameters.clef parameters with
    | None -> content
    | Some clef_parameter ->
      let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
      Str.global_replace clef_regex ("\\clef " ^ Music.clef_to_lilypond_string clef_parameter) content
  in
  let source, target =
    match Model.VersionParameters.transposition' parameters with
    | Relative (source, target) -> (source, target)
    | Absolute target -> (Music.key_pitch key, target)
  (* FIXME: Similarly to version.ml, probably need to fix an octave in Absolue *)
  in let source = Music.pitch_to_lilypond_string source in
  let target = Music.pitch_to_lilypond_string target in

  (* Create the Lilypond file *)
  Log.debug (fun m -> m "Generating Scheme & LilyPond string");
  let lilypond =
    Format.with_formatter_to_string @@ fun fmt ->
    fpf fmt [%blob "../template/lyversion.ly"];
    fpf fmt "#(load \"%s\")\n\n" (Filename.basename fname_scm);
    fpf fmt [%blob "../template/layout.ly"];
    fpf fmt [%blob "../template/paper.ly"];
    fpf fmt [%blob "../template/cropped.ly"];
    fpf fmt [%blob "../template/helpers.ly"];
    fpf fmt [%blob "../template/repeat_volta_fancy.ly"];
    fpf fmt [%blob "../template/bar_numbering/repeat_aware.ly"];
    fpf fmt [%blob "../template/bar_numbering/bar_number_in_instrument_name_engraver.ly"];
    fpf fmt [%blob "../template/bar_numbering/beginning_of_line.ly"];
    fpf fmt [%blob "../template/scottish_chords.ly"];
    fpf fmt [%blob "../template/fancy_unfold_repeats.ly"];
    fpf fmt [%blob "../template/header.ly"] title subtitle;
    fpf fmt [%blob "../template/version/header.ly"];
    fpf fmt [%blob "../template/version.ly"] piece opus source target content tempo_unit tempo_value (Kind.Base.to_pretty_string ~capitalised: false kind) source target content
  in
  let scheme =
    Format.with_formatter_to_string @@ fun fmt ->
    fpf fmt [%blob "../template/scheme/extlib.scm"];
    fpf fmt [%blob "../template/scheme/extlylib.scm"];
    fpf fmt [%blob "../template/scheme/get_partial.scm"];
    fpf fmt [%blob "../template/scheme/duration_of_music.scm"];
    fpf fmt [%blob "../template/scheme/skip_as_repeat.scm"];
    fpf fmt [%blob "../template/scheme/scottish_chords/jig_chords.scm"];
    fpf fmt [%blob "../template/scheme/scottish_chords/reel_chords.scm"];
    fpf fmt [%blob "../template/scheme/scottish_chords/waltz_chords.scm"];
    fpf fmt [%blob "../template/scheme/scottish_chords/chords.scm"];
    fpf fmt [%blob "../template/scheme/fancy_unfold_repeats/unfold_first_volta_repeat.scm"];
    fpf fmt [%blob "../template/scheme/fancy_unfold_repeats/extract_span.scm"];
    fpf fmt [%blob "../template/scheme/fancy_unfold_repeats/split_rhythmic_event_at.scm"];
    fpf fmt [%blob "../template/scheme/fancy_unfold_repeats/add_trailing_silence.scm"];
    fpf fmt [%blob "../template/scheme/fancy_unfold_repeats/fancy_unfold_repeats.scm"]
  in
  Log.debug (fun m -> m "Writing them to filesystem");
  Lwt_io.with_file
    ~mode: Output
    fname
    (fun ochan -> Lwt_io.write ochan lilypond);%lwt
  Lwt_io.with_file
    ~mode: Output
    fname_scm
    (fun ochan -> Lwt_io.write ochan scheme)
