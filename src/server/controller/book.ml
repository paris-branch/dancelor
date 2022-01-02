open Nes
open Dancelor_common
open Dancelor_server_model
module Log = (val Dancelor_server_logs.create "controller.book" : Logs.LOG)

module Ly = struct
  let render ?(parameters=BookParameters.none) book =
    let parameters = BookParameters.fill parameters in
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let%lwt title = Book.title book in (** FIXME: subtitle *)
      fpf fmt [%blob "template/lyversion.ly"];
      fpf fmt [%blob "template/book/macros.ly"];
      fpf fmt [%blob "template/layout.ly"];
      fpf fmt [%blob "template/book/globals.ly"]
        title (Option.unwrap_or ~default:"" parameters.instruments);
      fpf fmt [%blob "template/paper.ly"];

      fpf fmt [%blob "template/book/paper.ly"];
      if parameters |> BookParameters.two_sided then
        (
          fpf fmt [%blob "template/book/two-sided.ly"];
          fpf fmt
            (if parameters |> BookParameters.running_header then
               [%blob "template/book/header/two-sided.ly"]
             else
               [%blob "template/book/header/none.ly"])

        )
      else
        (
          fpf fmt
            (if parameters |> BookParameters.running_header then
               [%blob "template/book/header/one-sided.ly"]
             else
               [%blob "template/book/header/none.ly"])
        );
      fpf fmt [%blob "template/repeat-volta-fancy.ly"];
      fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
      fpf fmt [%blob "template/bar-numbering/bar-number-in-instrument-name-engraver.ly"];
      fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
      fpf fmt [%blob "template/book/book_beginning.ly"];
      if parameters |> BookParameters.front_page then
        fpf fmt [%blob "template/book/book_front_page.ly"];
      if parameters |> BookParameters.table_of_contents |> (=) BookParameters.Beginning then
        fpf fmt [%blob "template/book/book_table_of_contents.ly"];
      let%lwt () =
        let%lwt sets_and_parameters =
          let%lwt contents = Book.contents book in
          Lwt_list.map_p
            (function
              | Book.Version (version, parameters) ->
                let%lwt tune = Version.tune version in
                let%lwt name = Tune.name tune in
                let%lwt bars = Version.bars version in
                let%lwt kind = Tune.kind tune in
                let parameters = VersionParameters.set_display_name "" parameters in
                let%lwt set =
                  Set.make_temp ~name ~kind:(1, [bars, kind])
                    ~versions_and_parameters:[version, parameters]
                    ~order:[Internal 1]
                    ()
                in
                let%lwt for_dance = VersionParameters.for_dance parameters in
                let%lwt set_parameters = SetParameters.make ?for_dance ~show_order:false () in
                Lwt.return (set, set_parameters)

              | Set (set, parameters) | InlineSet (set, parameters) ->
                Lwt.return (set, parameters))
            contents
        in
        Lwt_list.iter_s
          (fun (set, set_parameters) ->
             let set_parameters = SetParameters.compose (BookParameters.every_set parameters) set_parameters in
             let%lwt name = Set.name set in
             let%lwt (kind, dance_and_kind) =
               match%lwt set_parameters |> SetParameters.for_dance with
               | None ->
                 let%lwt kind = Set.kind set in
                 let kind = Kind.dance_to_pretty_string kind in
                 Lwt.return (kind, kind)
               | Some dance ->
                 let%lwt name = Dance.name dance in
                 let%lwt kind = Dance.kind dance in
                 let kind = Kind.dance_to_pretty_string kind in
                 Lwt.return (kind, spf "Dance: %s — %s" name kind)
             in
             let%lwt deviser =
               if not (set_parameters |> SetParameters.show_deviser) then
                 Lwt.return ""
               else
                 (match%lwt Set.deviser set with
                  | None -> Lwt.return ""
                  | Some deviser ->
                    let%lwt deviser = Credit.line deviser in
                    Lwt.return (spf "Set by %s" deviser))
             in
             let%lwt dance_kind_and_order =
               if SetParameters.show_order set_parameters then
                 let%lwt order = Set.order set >|=| SetOrder.to_pretty_string in
                 Lwt.return (spf "%s — Play %s" dance_and_kind order)
               else
                 Lwt.return dance_and_kind
             in
             let%lwt dance_kind_order_and_chords =
               match%lwt SetParameters.for_dance set_parameters with
               | None -> Lwt.return dance_kind_and_order
               | Some dance ->
                 let%lwt two_chords = Dance.two_chords dance in
                 if two_chords then
                   Lwt.return (spf "%s — Two Chords" dance_kind_and_order)
                 else
                   Lwt.return dance_kind_and_order
             in
             fpf fmt [%blob "template/book/set_beginning.ly"]
               name kind name deviser dance_kind_order_and_chords;
             (match set_parameters |> SetParameters.forced_pages with
              | 0 -> ()
              | n -> fpf fmt [%blob "template/book/set-forced-pages.ly"] n);
             let%lwt () =
               let%lwt versions_and_parameters = Set.versions_and_parameters set in
               Lwt_list.iter_s
                 (fun (version, version_parameters) ->
                    let version_parameters = VersionParameters.compose (SetParameters.every_version set_parameters) version_parameters in
                    let%lwt content = Version.content version in
                    let content =
                      match version_parameters |> VersionParameters.clef with
                      | None -> content
                      | Some clef_parameter ->
                        let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
                        Str.global_replace clef_regex ("\\clef " ^ Music.clef_to_string clef_parameter) content
                    in
                    let%lwt tune = Version.tune version in
                    let%lwt key = Version.key version in
                    let%lwt name = Tune.name tune in
                    let name =
                      version_parameters
                      |> VersionParameters.display_name
                      |> Option.unwrap_or ~default:name
                    in
                    let%lwt author =
                      match%lwt Tune.author tune with
                      | None -> Lwt.return ""
                      | Some author -> Credit.line author
                    in
                    let author =
                      version_parameters
                      |> VersionParameters.display_author
                      |> Option.unwrap_or ~default:author
                    in
                    let first_bar =
                      version_parameters
                      |> VersionParameters.first_bar
                    in
                    let source, target =
                      match version_parameters |> VersionParameters.transposition with
                      | Relative (source, target) -> (source, target)
                      | Absolute target -> (key |> Music.key_pitch, target) (* FIXME: probably an octave to fix here *)
                    in
                    fpf fmt [%blob "template/book/version.ly"]
                      name author
                      first_bar
                      name
                      (Music.pitch_to_lilypond_string source)
                      (Music.pitch_to_lilypond_string target)
                      content;
                    Lwt.return ())
                 versions_and_parameters
             in
             fpf fmt [%blob "template/book/set_end.ly"];
             Lwt.return ())
          sets_and_parameters
      in
      if parameters |> BookParameters.table_of_contents |> (=) BookParameters.End then
        fpf fmt [%blob "template/book/book_table_of_contents.ly"];
      fpf fmt [%blob "template/book/book_end.ly"];
      Lwt.return ()
    in
    prom; %lwt
    Lwt.return res
end

module Pdf = struct
  let cache : ('a * Book.t, string Lwt.t) Cache.t = Cache.create ()

  let render ?parameters book =
    Cache.use
      cache (parameters, book)
      (fun () ->
        let%lwt lilypond = Ly.render ?parameters book in
        let path = Filename.concat !Dancelor_server_config.cache "book" in
        let%lwt (fname_ly, fname_pdf) =
          let%lwt slug = Book.slug book in
          let fname = aspf "%a-%x" Slug.pp slug (Random.int (1 lsl 29)) in
          Lwt.return (fname^".ly", fname^".pdf")
        in
        Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
          (fun ochan -> Lwt_io.write ochan lilypond); %lwt
        Log.debug (fun m -> m "Processing with LilyPond");
        LilyPond.run ~exec_path:path fname_ly; %lwt
        let path_pdf = Filename.concat path fname_pdf in
        Lwt.return path_pdf)

  let get book query_parameters =
    let%lwt book = Book.get book in
    let%lwt parameters =
      match QueryParameters.get "parameters" query_parameters with
      | None -> Lwt.return_none
      | Some parameters ->
        parameters
        |> BookParameters.of_yojson
        |> Result.get_ok
        |> Lwt.return_some
    in
    let%lwt path_pdf = render ?parameters book in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
