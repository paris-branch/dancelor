open Nes
open Dancelor_server_model
open QueryHelpers
module Log = (val Dancelor_server_logs.create "controller.program" : Logs.LOG)

module Ly = struct
  let render ?(parameters=ProgramParameters.none) program =
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let%lwt name = Program.name program in
      fpf fmt [%blob "template/lyversion.ly"];
      fpf fmt [%blob "template/program/macros.ly"];
      fpf fmt [%blob "template/layout.ly"];
      fpf fmt [%blob "template/program/globals.ly"]
        name (Parameter.get ~default:"" parameters.instruments);
      fpf fmt [%blob "template/paper.ly"];
      fpf fmt [%blob "template/program/paper.ly"];
      fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
      fpf fmt [%blob "template/bar-numbering/bar-number-in-instrument-name-engraver.ly"];
      fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
      fpf fmt [%blob "template/repeat-volta-fancy.ly"];
      fpf fmt [%blob "template/program/book_beginning.ly"];
      fpf fmt [%blob "template/program/book_front_page.ly"];
      let%lwt () =
        let%lwt sets_and_parameters = Program.sets_and_parameters program in
        Lwt_list.iter_s
          (fun (set, set_parameters) ->
             let set_parameters = SetParameters.compose ~parent:set_parameters (ProgramParameters.every_set parameters) in
             let%lwt name = Set.name set in
             let%lwt kind = Set.kind set in
             let kind = Kind.dance_to_string kind in
             fpf fmt [%blob "template/program/set_beginning.ly"]
               name kind name kind;
             let%lwt () =
               let%lwt versions_and_parameters = Set.versions_and_parameters set in
               Lwt_list.iter_s
                 (fun (version, version_parameters) ->
                    let version_parameters = VersionParameters.compose ~parent:version_parameters (SetParameters.every_version set_parameters) in
                    let%lwt content = Version.content version in
                    let content =
                      match version_parameters |> VersionParameters.clef with
                      | Parameter.Undefined -> content
                      | Parameter.Defined clef_parameter ->
                        let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
                        Str.global_replace clef_regex ("\\clef " ^ Music.clef_to_string clef_parameter) content
                    in
                    let%lwt tune = Version.tune version in
                    let%lwt key = Version.key version in
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
                    fpf fmt [%blob "template/program/version.ly"]
                      name author name source target content;
                    Lwt.return ())
                 versions_and_parameters
             in
             fpf fmt [%blob "template/program/set_end.ly"];
             Lwt.return ())
          sets_and_parameters
      in
      fpf fmt [%blob "template/program/book_end.ly"];
      Lwt.return ()
    in
    prom; %lwt
    Lwt.return res
end

module Pdf = struct
  let cache : ('a * Program.t, string Lwt.t) Cache.t = Cache.create ()

  let render ?parameters program =
    Cache.use
      cache (parameters, program)
      (fun () ->
        let%lwt lilypond = Ly.render ?parameters program in
        let path = Filename.concat !Dancelor_server_config.cache "program" in
        let%lwt (fname_ly, fname_pdf) =
          let%lwt slug = Program.slug program in
          let fname = spf "%s-%x" slug (Random.int (1 lsl 29)) in
          Lwt.return (fname^".ly", fname^".pdf")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond); %lwt
        Log.debug (fun m -> m "Processing with LilyPond");
        LilyPond.run ~exec_path:path fname_ly; %lwt
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get program query =
    let%lwt program = Program.get program in
    let%lwt parameters =
      match%lwt query_string_opt query "parameters" with
      | None -> Lwt.return_none
      | Some parameters ->
        parameters
        |> Yojson.Safe.from_string
        |> ProgramParameters.of_yojson
        |> Result.get_ok
        |> Lwt.return_some
    in
    let%lwt path_pdf = render ?parameters program in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
