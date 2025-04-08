open NesUnix
open Common

module Log = (val Logger.create "controller.version": Logs.LOG)

let get_ly version =
  let%lwt version = Model.Version.get version in
  let body = Model.Version.content version in
  Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_string ~status: `OK ~body ()

let prepare_ly_file parameters ?(show_meta = false) ?(meta_in_title = false) ~fname version =
  Log.debug (fun m -> m "Preparing Lilypond file");
  let fname_scm = Filename.chop_extension fname ^ ".scm" in
  let%lwt tune = Model.Version.tune version in
  let key = Model.Version.key version in
  let name = Model.VersionParameters.display_name' ~default: (Model.Tune.name tune) parameters in
  let%lwt composer = Lwt.map (String.concat ", " ~last: " and " % List.map Model.Person.name) (Model.Tune.composers tune) in
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
  let kind = Model.Tune.kind tune in
  let (tempo_unit, tempo_value) = Kind.Base.tempo kind in
  Log.debug (fun m -> m "Getting content");
  let content = Model.Version.content version in

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
    fpf fmt [%blob "template/lyversion.ly"];
    fpf fmt "#(load \"%s\")\n\n" (Filename.basename fname_scm);
    fpf fmt [%blob "template/layout.ly"];
    fpf fmt [%blob "template/paper.ly"];
    fpf fmt [%blob "template/cropped.ly"];
    fpf fmt [%blob "template/helpers.ly"];
    fpf fmt [%blob "template/repeat_volta_fancy.ly"];
    fpf fmt [%blob "template/bar_numbering/repeat_aware.ly"];
    fpf fmt [%blob "template/bar_numbering/bar_number_in_instrument_name_engraver.ly"];
    fpf fmt [%blob "template/bar_numbering/beginning_of_line.ly"];
    fpf fmt [%blob "template/scottish_chords.ly"];
    fpf fmt [%blob "template/fancy_unfold_repeats.ly"];
    fpf fmt [%blob "template/header.ly"] title subtitle;
    fpf fmt [%blob "template/version/header.ly"];
    fpf fmt [%blob "template/version.ly"] piece opus source target content tempo_unit tempo_value (Kind.Base.to_pretty_string ~capitalised: false kind) source target content
  in
  let scheme =
    Format.with_formatter_to_string @@ fun fmt ->
    fpf fmt [%blob "template/scheme/extlib.scm"];
    fpf fmt [%blob "template/scheme/extlylib.scm"];
    fpf fmt [%blob "template/scheme/get_partial.scm"];
    fpf fmt [%blob "template/scheme/duration_of_music.scm"];
    fpf fmt [%blob "template/scheme/skip_as_repeat.scm"];
    fpf fmt [%blob "template/scheme/scottish_chords/jig_chords.scm"];
    fpf fmt [%blob "template/scheme/scottish_chords/reel_chords.scm"];
    fpf fmt [%blob "template/scheme/scottish_chords/waltz_chords.scm"];
    fpf fmt [%blob "template/scheme/scottish_chords/chords.scm"];
    fpf fmt [%blob "template/scheme/fancy_unfold_repeats/unfold_first_volta_repeat.scm"];
    fpf fmt [%blob "template/scheme/fancy_unfold_repeats/extract_span.scm"];
    fpf fmt [%blob "template/scheme/fancy_unfold_repeats/split_rhythmic_event_at.scm"];
    fpf fmt [%blob "template/scheme/fancy_unfold_repeats/add_trailing_silence.scm"];
    fpf fmt [%blob "template/scheme/fancy_unfold_repeats/fancy_unfold_repeats.scm"]
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

let populate_cache ~cache ~ext ~pp_ext =
  Log.info (fun m -> m "Populating the version %s cache" pp_ext);
  let path = Filename.concat !Config.cache "version" in
  let files = Lwt_unix.files_of_directory path in
  Lwt_stream.iter
    (fun x ->
      if Filename.check_suffix x ext then
        try
          Log.debug (fun m -> m "Found %s file %s" pp_ext x);
          let base = Filename.chop_suffix x ext in
          let hash =
            String.split_on_char '-' base
            |> List.ft
            |> StorageCache.hash_from_string
          in
          StorageCache.add ~cache ~hash ~value: (Lwt.return (Filename.concat path x))
        with
          | exn ->
            Log.err (fun m ->
              m
                "%a"
                (Format.pp_multiline_sensible ("Could not determine hash from file `" ^ x ^ "`"))
                ((Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ()))
            );
            exit 7
    )
    files

module Svg = struct
  let cache : ([`Svg] * Model.Version.t Entry.t * Model.VersionParameters.t * string, string Lwt.t) StorageCache.t =
    StorageCache.create ()

  let populate_cache () =
    populate_cache ~cache ~ext: ".svg" ~pp_ext: "svg"

  let render parameters version =
    let body = Model.Version.content version in
    StorageCache.use ~cache ~key: (`Svg, version, parameters, body) @@ fun hash ->
    Log.debug (fun m -> m "Rendering the LilyPond version");
    let%lwt (fname_ly, fname_svg) =
      let slug = Entry.slug version in
      let fname = aspf "%a-%a" Slug.pp' slug StorageCache.pp_hash hash in
      Lwt.return (fname ^ ".ly", fname ^ ".svg")
    in
    Log.debug (fun m -> m "LilyPond file name: %s" fname_ly);
    Log.debug (fun m -> m "SVG file name: %s" fname_svg);
    let path = Filename.concat !Config.cache "version" in
    Log.debug (fun m -> m "Preparing lilypond file");
    prepare_ly_file parameters ~show_meta: false ~fname: (Filename.concat path fname_ly) version;%lwt
    Log.debug (fun m -> m "Generate score");
    LilyPond.svg
      ~exec_path: path
      ~fontconfig_file: (Filename.concat !Config.share "fonts.conf")
      ~stylesheet: "/fonts.css"
      fname_ly;%lwt
    Log.debug (fun m -> m "done!");
    Lwt.return (Filename.concat path fname_svg)

  let get parameters version =
    Log.debug (fun m -> m "Model.Version.Svg.get %a" Slug.pp' version);
    let%lwt version = Model.Version.get version in
    let%lwt path_svg = render parameters version in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_svg ()

  let preview_slug = Slug.check_string_exn "preview"
  let preview parameters version =
    Log.debug (fun m -> m "Model.Version.Svg.preview");
    let version = Entry.make ~slug: preview_slug version in
    let%lwt path_svg = render parameters version in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_svg ()
end

module Pdf = struct
  let render parameters version =
    let%lwt kind = Model.Version.kind version in
    let%lwt name = Model.Version.name version in
    let%lwt set_parameters =
      Model.SetParameters.make
        ~show_order: false
        ?display_name: (Model.VersionParameters.display_name parameters)
        ()
    in
    let parameters = Model.VersionParameters.set_display_name "" parameters in
    let set =
      Entry.make_dummy @@
        Model.Set.make
          ~name
          ~kind: (Kind.Dance.Version kind)
          ~contents: [(version, parameters)]
          ~order: [Internal 1]
          ()
    in
    Set.Pdf.render set_parameters set

  let get parameters version =
    let%lwt version = Model.Version.get version in
    let%lwt path_pdf = render parameters version in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
end

module Ogg = struct
  let cache : ([`Ogg] * Model.Version.t Entry.t * Model.VersionParameters.t * string, string Lwt.t) StorageCache.t =
    StorageCache.create ()

  let populate_cache () =
    populate_cache ~cache ~ext: ".ogg" ~pp_ext: "ogg"

  let render parameters version =
    let body = Model.Version.content version in
    StorageCache.use ~cache ~key: (`Ogg, version, parameters, body) @@ fun hash ->
    let%lwt (fname_ly, fname_ogg) =
      let slug = Entry.slug version in
      let fname = aspf "%a-%a" Slug.pp' slug StorageCache.pp_hash hash in
      Lwt.return (fname ^ ".ly", fname ^ ".ogg")
    in
    let path = Filename.concat !Config.cache "version" in
    prepare_ly_file ~fname: (Filename.concat path fname_ly) parameters version;%lwt
    Log.debug (fun m -> m "Processing with LilyPond");
    LilyPond.ogg
      ~exec_path: path
      ~fontconfig_file: (Filename.concat !Config.share "fonts.conf")
      fname_ly;%lwt
    Lwt.return (Filename.concat path fname_ogg)

  let get parameters version =
    Log.debug (fun m -> m "Model.Version.Ogg.get %a" Slug.pp' version);
    let%lwt version = Model.Version.get version in
    let%lwt path_ogg = render parameters version in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_ogg ()

  let preview_slug = Slug.check_string_exn "preview"
  let preview parameters version =
    Log.debug (fun m -> m "Model.Version.Ogg.preview");
    let version = Entry.make ~slug: preview_slug version in
    let%lwt path_ogg = render parameters version in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_ogg ()
end

let dispatch : type a r. (a, r Lwt.t, r) Endpoints.Version.t -> a = function
  | Get -> Model.Version.get
  | Search -> Model.Version.search
  | Create -> Model.Version.create
  | Update -> Model.Version.update
  | Ly -> get_ly
  | Svg -> Svg.get
  | Ogg -> Ogg.get
  | Pdf -> Pdf.get
  | PreviewSvg -> Svg.preview
  | PreviewOgg -> Ogg.preview
