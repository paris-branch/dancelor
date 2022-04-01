open Nes
open Dancelor_common
open Dancelor_server_model
module Log = (val Dancelor_server_logs.create "controller.version" : Logs.LOG)

let get_ly version _ =
  let%lwt version = Version.get version in
  let%lwt body = Version.content version in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

let prepare_ly_file ?(parameters=VersionParameters.none) ?(show_meta=false) ?(meta_in_title=false) ~fname version =
  Log.debug (fun m -> m "Preparing Lilypond file");
  let parameters = VersionParameters.fill parameters in

  let fname_scm = Filename.chop_extension fname ^ ".scm" in
  let%lwt tune = Version.tune version in
  let%lwt key = Version.key version in
  let%lwt name = Tune.name tune in
  let name =
    parameters
    |> VersionParameters.display_name
    |> Option.unwrap_or ~default:name
  in
  let%lwt author =
    match%lwt Tune.author tune with
    | None -> Lwt.return ""
    | Some author -> Credit.line author
  in
  let author =
    parameters
    |> VersionParameters.display_author
    |> Option.unwrap_or ~default:author
  in
  let title, piece =
    if show_meta then
      if meta_in_title then name, " " else "", name
    else
      "", ""
  in
  let subtitle, opus =
    if show_meta then
      if meta_in_title then author, " " else "", author
    else
      "", ""
  in
  let%lwt kind = Tune.kind tune in
  let (tempo_unit, tempo_value) = Kind.base_tempo kind in
  Log.debug (fun m -> m "Getting content");
  let%lwt content = Version.content version in

  (* Handle parameters *)

  let content =
    match parameters |> VersionParameters.clef with
    | None -> content
    | Some clef_parameter ->
      let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
      Str.global_replace clef_regex ("\\clef " ^ Music.clef_to_lilypond_string clef_parameter) content
  in
  let source, target =
    match parameters |> VersionParameters.transposition with
    | Relative (source, target) -> (source, target)
    | Absolute target -> (Music.key_pitch key, target)
    (* FIXME: Similarly to version.ml, probably need to fix an octave in Absolue *)
  in
  let source = Music.pitch_to_lilypond_string source in
  let target = Music.pitch_to_lilypond_string target in

  (* Create the Lilypond file *)
  Log.debug (fun m -> m "Generating Scheme & LilyPond string");
  let lilypond =
    Format.with_formatter_to_string @@ fun fmt ->
    fpf fmt [%blob "template/lyversion.ly"];
    fpf fmt "#(load \"%s\")\n\n" (Filename.basename fname_scm);
    fpf fmt [%blob "template/layout.ly"];
    fpf fmt [%blob "template/paper.ly"];
    fpf fmt [%blob "template/repeat-volta-fancy.ly"];
    fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
    fpf fmt [%blob "template/bar-numbering/bar-number-in-instrument-name-engraver.ly"];
    fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
    fpf fmt [%blob "template/scottish-chords.ly"];
    fpf fmt [%blob "template/fancy-unfold-repeats.ly"];
    fpf fmt [%blob "template/header.ly"] title subtitle;

    fpf fmt [%blob "template/version/header.ly"];
    fpf fmt [%blob "template/version.ly"] piece opus source target content tempo_unit tempo_value (Kind.base_to_pretty_string ~capitalised:false kind) source target content
  in
  let scheme =
    Format.with_formatter_to_string @@ fun fmt ->
    fpf fmt [%blob "template/scheme/extlib.scm"];
    fpf fmt [%blob "template/scheme/extlylib.scm"];
    fpf fmt [%blob "template/scheme/get-partial.scm"];
    fpf fmt [%blob "template/scheme/duration-of-music.scm"];
    fpf fmt [%blob "template/scheme/skip-as-repeat.scm"];
    fpf fmt [%blob "template/scheme/scottish-chords/jig-chords.scm"];
    fpf fmt [%blob "template/scheme/scottish-chords/reel-chords.scm"];
    fpf fmt [%blob "template/scheme/scottish-chords/waltz-chords.scm"];
    fpf fmt [%blob "template/scheme/scottish-chords/chords.scm"];
    fpf fmt [%blob "template/scheme/fancy-unfold-repeats/unfold-first-volta-repeat.scm"];
    fpf fmt [%blob "template/scheme/fancy-unfold-repeats/extract-span.scm"];
    fpf fmt [%blob "template/scheme/fancy-unfold-repeats/split-rhythmic-event-at.scm"];
    fpf fmt [%blob "template/scheme/fancy-unfold-repeats/add-trailing-silence.scm"];
    fpf fmt [%blob "template/scheme/fancy-unfold-repeats/fancy-unfold-repeats.scm"]
  in
  Log.debug (fun m -> m "Writing them to filesystem");
  Lwt_io.with_file ~mode:Output fname
    (fun ochan -> Lwt_io.write ochan lilypond);%lwt
  Lwt_io.with_file ~mode:Output fname_scm
    (fun ochan -> Lwt_io.write ochan scheme)

module Svg = struct
  let cache : (Version.t * VersionParameters.t option, string Lwt.t) Cache.t = Cache.create ()

  let render ?parameters version =
    Cache.use ~cache ~key:(version, parameters) @@ fun () ->
    Log.debug (fun m -> m "Rendering the LilyPond version");
    let%lwt (fname_ly, fname_svg) =
      let%lwt slug = Version.slug version in
      let fname = aspf "%a-%x" Slug.pp slug (Random.int (1 lsl 29)) in
      Lwt.return (fname^".ly", fname^".cropped.svg")
    in
    Log.debug (fun m -> m "LilyPond file name: %s" fname_ly);
    Log.debug (fun m -> m "SVG file name: %s" fname_svg);
    let path = Filename.concat !Dancelor_server_config.cache "version" in
    Log.debug (fun m -> m "Preparing lilypond file");
    prepare_ly_file ?parameters ~show_meta:false ~fname:(Filename.concat path fname_ly) version;%lwt
    Log.debug (fun m -> m "Generate score and crop");
    LilyPond.cropped_svg ~exec_path:path fname_ly;%lwt
    Log.debug (fun m -> m "done!");
    Lwt.return (Filename.concat path fname_svg)

  let get version query_parameters =
    Log.debug (fun m -> m "Version.Svg.get %a" Slug.pp version);
    let%lwt version = Version.get version in
    let%lwt parameters =
      let%olwt parameters = Lwt.return (QueryParameters.get "parameters" query_parameters) in
      parameters
      |> VersionParameters.of_yojson
      |> Result.get_ok
      |> Lwt.return_some
    in
    let%lwt path_svg = render ?parameters version in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_svg ()
end

module Pdf = struct
  let cache : (Version.t * VersionParameters.t option, string Lwt.t) Cache.t = Cache.create ()

  let render ?parameters version =
    Cache.use ~cache ~key:(version, parameters) @@ fun () ->
    let%lwt (fname_ly, fname_pdf) =
      let%lwt slug = Version.slug version in
      let fname = aspf "%a-%x-with-meta" Slug.pp slug (Random.int (1 lsl 29)) in
      Lwt.return (fname^".ly", fname^".pdf")
    in
    let path = Filename.concat !Dancelor_server_config.cache "version" in
    prepare_ly_file ?parameters ~show_meta:true ~meta_in_title:true
      ~fname:(Filename.concat path fname_ly) version;%lwt
    Log.debug (fun m -> m "Processing with LilyPond");
    LilyPond.run ~exec_path:path fname_ly;%lwt
    Lwt.return (Filename.concat path fname_pdf)

  let get version query_parameters =
    Log.debug (fun m -> m "Version.pdf.get %a" Slug.pp version);
    let%lwt version = Version.get version in
    let%lwt parameters =
      let%olwt parameters = Lwt.return (QueryParameters.get "parameters" query_parameters) in
      parameters
      |> VersionParameters.of_yojson
      |> Result.get_ok
      |> Lwt.return_some
    in
    let%lwt path_pdf = render ?parameters version in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end

module Ogg = struct
  let cache : (Version.t * VersionParameters.t option, string Lwt.t) Cache.t = Cache.create ()

  let render ?parameters version =
    Cache.use ~cache ~key:(version, parameters) @@ fun () ->
    let%lwt (fname_ly, fname_ogg) =
      let%lwt slug = Version.slug version in
      let fname = aspf "%a-%x" Slug.pp slug (Random.int (1 lsl 29)) in
      Lwt.return (fname^".ly", fname^".ogg")
    in
    let path = Filename.concat !Dancelor_server_config.cache "version" in
    prepare_ly_file ~fname:(Filename.concat path fname_ly) version;%lwt
    Log.debug (fun m -> m "Processing with LilyPond");
    LilyPond.ogg ~exec_path:path fname_ly;%lwt
    Lwt.return (Filename.concat path fname_ogg)

  let get version _ =
    Log.debug (fun m -> m "Version.Ogg.get %a" Slug.pp version);
    let%lwt version = Version.get version in
    let%lwt path_ogg = render version in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_ogg ()
end
