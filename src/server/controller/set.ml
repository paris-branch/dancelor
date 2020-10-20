open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.set" : Logs.LOG)

module Ly = struct

  let render ?(parameters=SetParameters.none) set =
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let%lwt title = Set.name set in
      let%lwt kind = Set.kind set in
      let%lwt versions_and_parameters = Set.versions_and_parameters set in
      fpf fmt [%blob "template/lyversion.ly"];
      fpf fmt [%blob "template/layout.ly"];
      fpf fmt [%blob "template/paper.ly"];
      fpf fmt [%blob "template/set/paper.ly"];
      fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
      fpf fmt [%blob "template/bar-numbering/bar-number-in-instrument-name-engraver.ly"];
      fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
      fpf fmt [%blob "template/repeat-volta-fancy.ly"];
      fpf fmt [%blob "template/set/header.ly"]
        title (Kind.dance_to_string kind)
        (Parameter.get ~default:"" parameters.instruments);
      Lwt_list.iter_s
        (fun (version, version_parameters) ->
           let version_parameters = VersionParameters.compose ~parent:version_parameters (SetParameters.every_version parameters) in
           let%lwt content = Version.content version in
           let%lwt key = Version.key version in
           let%lwt tune = Version.tune version in
           let%lwt name = Tune.name tune in
           let%lwt author =
             match%lwt Tune.author tune with
             | None -> Lwt.return ""
             | Some author -> Credit.line author
           in
           let source, target =
             match version_parameters |> VersionParameters.transposition |> Parameter.get ~default:Transposition.identity with
             | Relative (source, target) -> (source, target)
             | Absolute target -> (key |> fst |> Music.pitch_to_string, target)
           in
           fpf fmt [%blob "template/set/version.ly"]
             name author
             source target
             content;
           Lwt.return ())
        versions_and_parameters
    in
    prom; %lwt
    Lwt.return res

  let get set query =
    let%lwt set = Set.get set in
    let%lwt parameters =
      match%lwt query_string_opt query "parameters" with
      | None -> Lwt.return_none
      | Some parameters ->
        parameters
        |> Yojson.Safe.from_string
        |> SetParameters.of_yojson
        |> Result.get_ok
        |> Lwt.return_some
    in
    let%lwt lilypond = render ?parameters set in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:lilypond ()
end

module Pdf = struct
  let cache : (('a * Set.t), string Lwt.t) Cache.t = Cache.create ()

  let render ?parameters set =
    Cache.use
      cache (parameters, set)
      (fun () ->
        let%lwt lilypond = Ly.render ?parameters set in
        let path = Filename.concat !Dancelor_server_config.cache "set" in
        let%lwt (fname_ly, fname_pdf) =
          let%lwt slug = Set.slug set in
          let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
          Lwt.return (fname^".ly", fname^".pdf")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond); %lwt
        Log.debug (fun m -> m "Processing with LilyPond");
        LilyPond.run ~exec_path:path fname_ly; %lwt
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get set query =
    let%lwt set = Set.get set in
    let%lwt parameters =
      match%lwt query_string_opt query "parameters" with
      | None -> Lwt.return_none
      | Some parameters ->
        parameters
        |> Yojson.Safe.from_string
        |> SetParameters.of_yojson
        |> Result.get_ok
        |> Lwt.return_some
    in
    let%lwt path_pdf = render ?parameters set in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
