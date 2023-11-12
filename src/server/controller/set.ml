open NesUnix
module Model = Dancelor_server_model
module Log = (val Dancelor_server_logs.create "controller.set" : Logs.LOG)

module Ly = struct
  let cache : ([`Ly] * Model.Set.t * Model.SetParameters.t * string, string Lwt.t) StorageCache.t = StorageCache.create ()

  let render ?(parameters=Model.SetParameters.none) set =
    let%lwt body = Model.Set.lilypond_content_cache_key set in
    StorageCache.use ~cache ~key:(`Ly, set, parameters, body) @@ fun _hash ->
    let parameters = Model.SetParameters.fill parameters in
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let%lwt title = Model.Set.name set in
      let title = Option.value ~default:title @@ Model.SetParameters.display_name parameters in
      let%lwt kind = Model.Set.kind set in
      let%lwt versions_and_parameters = Model.Set.versions_and_parameters set in
      fpf fmt [%blob "template/lyversion.ly"];
      fpf fmt [%blob "template/layout.ly"];
      fpf fmt [%blob "template/paper.ly"];
      fpf fmt [%blob "template/set/paper.ly"];
      fpf fmt [%blob "template/repeat-volta-fancy.ly"];
      fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
      fpf fmt [%blob "template/bar-numbering/bar-number-in-instrument-name-engraver.ly"];
      fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
      fpf fmt [%blob "template/set/header.ly"]
        title (Model.Kind.Dance.to_string kind)
        (Option.unwrap_or ~default:"" (Model.SetParameters.instruments parameters));
      Lwt_list.iter_s
        (fun (version, version_parameters) ->
           let version_parameters = Model.VersionParameters.compose (Model.SetParameters.every_version parameters) version_parameters in
           let%lwt content = Model.Version.content version in
           let content =
             match version_parameters |> Model.VersionParameters.clef with
             | None -> content
             | Some clef_parameter ->
               let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
               Str.global_replace clef_regex ("\\clef " ^ Model.Music.clef_to_lilypond_string clef_parameter) content
           in
           let%lwt key = Model.Version.key version in
           let%lwt tune = Model.Version.tune version in
           let%lwt name = Model.Tune.name tune in
           let name =
             version_parameters
             |> Model.VersionParameters.display_name
             |> Option.unwrap_or ~default:name
           in
           let%lwt author =
             match%lwt Model.Tune.author tune with
             | None -> Lwt.return ""
             | Some author -> Model.Credit.line author
           in
           let author =
             version_parameters
             |> Model.VersionParameters.display_author
             |> Option.unwrap_or ~default:author
           in
           let first_bar =
             version_parameters
             |> Model.VersionParameters.first_bar
           in
           let source, target =
             match version_parameters |> Model.VersionParameters.transposition with
             | Relative (source, target) -> (source, target)
             | Absolute target -> (Model.Music.key_pitch key, target) (* FIXME: probably an octave to fix here*)
           in
           fpf fmt [%blob "template/set/version.ly"]
             name author first_bar
             (Model.Music.pitch_to_lilypond_string source)
             (Model.Music.pitch_to_lilypond_string target)
             content;
           Lwt.return ())
        versions_and_parameters
    in
    prom;%lwt
    Lwt.return res
end

let populate_cache ~cache ~ext ~pp_ext =
  Log.info (fun m -> m "Populating the set %s cache" pp_ext);
  let path = Filename.concat !Dancelor_server_config.cache "set" in
  let files = Lwt_unix.files_of_directory path in
  Lwt_stream.iter (fun x ->
      if Filename.check_suffix x ext then
        try
          Log.debug (fun m -> m "Found %s file %s" pp_ext x);
          let base = Filename.chop_suffix x ext in
          let hash =
            String.split_on_char '-' base
            |> List.ft
            |> StorageCache.hash_from_string
          in
          StorageCache.add ~cache ~hash ~value:(Lwt.return (Filename.concat path x))
        with
          exn ->
          Log.err (fun m ->
              m "%a"
                (Format.pp_multiline_sensible ("Could not determine hash from file `" ^ x ^ "`"))
                ((Printexc.to_string exn) ^ "\n" ^ (Printexc.get_backtrace ())));
          exit 7
    ) files

module Pdf = struct
  let cache : ([`Pdf] * Model.Set.t * Model.SetParameters.t option * string, string Lwt.t) StorageCache.t =
    StorageCache.create ()

  let populate_cache () =
    populate_cache ~cache ~ext:".pdf" ~pp_ext:"pdf"

  let render ?parameters set =
    let%lwt body = Model.Set.lilypond_content_cache_key set in
    StorageCache.use ~cache ~key:(`Pdf, set, parameters, body) @@ fun hash ->
    let%lwt lilypond = Ly.render ?parameters set in
    let path = Filename.concat !Dancelor_server_config.cache "set" in
    let%lwt (fname_ly, fname_pdf) =
      let%lwt slug = Model.Set.slug set in
      let fname = aspf "%a-%a" Slug.pp slug StorageCache.pp_hash hash in
      Lwt.return (fname^".ly", fname^".pdf")
    in
    Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
      (fun ochan -> Lwt_io.write ochan lilypond);%lwt
    Log.debug (fun m -> m "Processing with LilyPond");
    LilyPond.run ~exec_path:path fname_ly;%lwt
    let path_pdf = Filename.concat path fname_pdf in
    Lwt.return path_pdf

  let get set parameters =
    let%lwt set = Model.Set.get set in
    let%lwt path_pdf = render ?parameters set in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
