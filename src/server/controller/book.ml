open NesUnix
open Dancelor_server_model
module Log = (val Dancelor_server_logs.create "controller.book" : Logs.LOG)

module Ly = struct

  let kind set set_parmeters =
    let%lwt kind =
      match%lwt SetParameters.for_dance set_parmeters with
      | None -> Set.kind set
      | Some dance -> Dance.kind dance
    in
    Lwt.return (Kind.Dance.to_pretty_string kind)

  let details_line set set_parameters =
    let%lwt dance =
      match%lwt SetParameters.for_dance set_parameters with
      | None -> Lwt.return_nil
      | Some dance ->
        let%lwt name = Dance.name dance in
        Lwt.return [spf "Dance: %s" name]
    in
    let%lwt kind = kind set set_parameters in
    let%lwt order =
      if SetParameters.show_order set_parameters then
        let%lwt order = Set.order set >|=| SetOrder.to_pretty_string in
        Lwt.return [spf "Play %s" order]
      else
        Lwt.return_nil
    in
    let%lwt chords =
      match%lwt SetParameters.for_dance set_parameters with
      | None -> Lwt.return_nil
      | Some dance ->
        let%lwt two_chords = Dance.two_chords dance in
        if two_chords then
          Lwt.return ["Two Chords"]
        else
          Lwt.return_nil
    in
    Lwt.return (String.concat " — " (dance @ [kind] @ order @ chords))

  let cache : ([`Ly] * Book.t * BookParameters.t * string, string Lwt.t) StorageCache.t =
    StorageCache.create ()

  let render ?(parameters=BookParameters.none) book =
    let%lwt body = Book.lilypond_contents_cache_key book in
    StorageCache.use ~cache ~key:(`Ly, book, parameters, body) @@ fun _hash ->
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
                let name =
                  parameters
                  |> VersionParameters.display_name
                  |> Option.unwrap_or ~default:name
                in
                let trivia =
                  parameters
                  |> VersionParameters.trivia
                  |> Option.unwrap_or ~default:" "
                in
                let%lwt bars = Version.bars version in
                let%lwt kind = Tune.kind tune in
                let parameters = VersionParameters.set_display_name trivia parameters in
                let%lwt set =
                  Set.make_temp ~name ~kind:(1, [bars, kind])
                    ~versions_and_parameters:[version, parameters]
                    ~order:[Internal 1]
                    ~modified_at:(NesDatetime.now ())
                    ~created_at:(NesDatetime.now ())
                    ()
                in
                let%lwt for_dance = VersionParameters.for_dance parameters in
                let%lwt set_parameters = SetParameters.make
                    ~display_name:name ?for_dance ~show_order:false () in
                Lwt.return (set, set_parameters)

              | Set (set, parameters) | InlineSet (set, parameters) ->
                Lwt.return (set, parameters))
            contents
        in
        Lwt_list.iter_s
          (fun (set, set_parameters) ->
             let set_parameters = SetParameters.compose (BookParameters.every_set parameters) set_parameters in
             let%lwt name = Set.name set in
             let name =
               set_parameters
               |> SetParameters.display_name
               |> Option.unwrap_or ~default:name
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
             let%lwt kind = kind set set_parameters in
             let%lwt details_line = details_line set set_parameters in
             fpf fmt [%blob "template/book/set_beginning.ly"]
               name kind name deviser details_line;
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
    prom;%lwt
    Lwt.return res
end

let populate_cache ~cache ~ext ~pp_ext =
  Log.info (fun m -> m "Populating the book %s cache" pp_ext);
  let path = Filename.concat !Dancelor_server_config.cache "book" in
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
  let cache : ([`Pdf] * Book.t * BookParameters.t option * string, string Lwt.t) StorageCache.t =
    StorageCache.create ()

  let populate_cache () =
    populate_cache ~cache ~ext:".pdf" ~pp_ext:"pdf"

  let render ?parameters book =
    let%lwt body = Book.lilypond_contents_cache_key book in
    StorageCache.use ~cache ~key:(`Pdf, book, parameters, body) @@ fun hash ->
    let%lwt lilypond = Ly.render ?parameters book in
    let path = Filename.concat !Dancelor_server_config.cache "book" in
    let%lwt (fname_ly, fname_pdf) =
      let%lwt slug = Book.slug book in
      let fname = aspf "%a-%a" Slug.pp slug StorageCache.pp_hash hash in
      Lwt.return (fname^".ly", fname^".pdf")
    in
    Lwt_io.with_file ~mode:Output (Filename.concat path fname_ly)
      (fun ochan -> Lwt_io.write ochan lilypond);%lwt
    Log.debug (fun m -> m "Processing with LilyPond");
    LilyPond.run ~exec_path:path fname_ly;%lwt
    let path_pdf = Filename.concat path fname_pdf in
    Lwt.return path_pdf

  let get book parameters =
    let%lwt book = Book.get book in
    let%lwt path_pdf = render ?parameters book in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
