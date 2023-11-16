open NesUnix
module Model = Dancelor_server_model
module Log = (val Dancelor_server_logs.create "controller.book" : Logs.LOG)

module Ly = struct

  let kind set set_parmeters =
    let%lwt kind =
      match%lwt Model.SetParameters.for_dance set_parmeters with
      | None -> Model.Set.kind set
      | Some dance -> Model.Dance.kind dance
    in
    Lwt.return (Model.Kind.Dance.to_pretty_string kind)

  let details_line set set_parameters =
    let%lwt dance =
      match%lwt Model.SetParameters.for_dance set_parameters with
      | None -> Lwt.return_nil
      | Some dance ->
        let%lwt name = Model.Dance.name dance in
        Lwt.return [spf "Dance: %s" name]
    in
    let%lwt kind = kind set set_parameters in
    let%lwt order =
      if Model.SetParameters.show_order set_parameters then
        let%lwt order = Model.Set.order set >|=| Model.SetOrder.to_pretty_string in
        Lwt.return [spf "Play %s" order]
      else
        Lwt.return_nil
    in
    let%lwt chords =
      match%lwt Model.SetParameters.for_dance set_parameters with
      | None -> Lwt.return_nil
      | Some dance ->
        let%lwt two_chords = Model.Dance.two_chords dance in
        if two_chords then
          Lwt.return ["Two Chords"]
        else
          Lwt.return_nil
    in
    Lwt.return (String.concat " — " (dance @ [kind] @ order @ chords))

  (** Rearrange the content of a set. [Default] will leave the content as-is,
      while [Unfolded] will duplicated the tunes depending on the set order. *)
  let rearrange_set_content ?(order_type=Model.SetParameters.Default) ~order content =
    let module P = Model.SetParameters in
    let module O = Model.SetOrder in
    match order_type with
    | P.Default -> content
    | P.Unfolded ->
      order
      |> List.map_filter (function O.Internal n -> Some n | _ -> None)
      |> List.map (List.nth content % Fun.flip (-) 1)

  let cache : ([`Ly] * Model.Book.t * Model.BookParameters.t * string, string Lwt.t) StorageCache.t =
    StorageCache.create ()

  let render ?(parameters=Model.BookParameters.none) book =
    let%lwt body = Model.Book.lilypond_contents_cache_key book in
    StorageCache.use ~cache ~key:(`Ly, book, parameters, body) @@ fun _hash ->
    let parameters = Model.BookParameters.fill parameters in
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let%lwt title = Model.Book.title book in (** FIXME: subtitle *)
      fpf fmt [%blob "template/lyversion.ly"];
      (
        match Model.BookParameters.paper_size parameters with
        | A n -> fpf fmt [%blob "template/paper-size/a.ly"] n;
        | Custom (width, height, unit) -> fpf fmt [%blob "template/paper-size/custom.ly"] width unit height unit;
      );
      fpf fmt [%blob "template/book/macros.ly"];
      fpf fmt [%blob "template/layout.ly"];
      fpf fmt [%blob "template/book/globals.ly"]
        title (Option.value ~default:"" (Model.BookParameters.instruments parameters));
      fpf fmt [%blob "template/paper.ly"];

      fpf fmt [%blob "template/book/paper.ly"];
      if parameters |> Model.BookParameters.two_sided then
        (
          fpf fmt [%blob "template/book/two-sided.ly"];
          fpf fmt
            (if parameters |> Model.BookParameters.running_header then
               [%blob "template/book/header/two-sided.ly"]
             else
               [%blob "template/book/header/none.ly"]);
          fpf fmt
            (if parameters |> Model.BookParameters.running_footer then
               [%blob "template/book/footer/two-sided.ly"]
             else
               [%blob "template/book/footer/none.ly"])
        )
      else
        (
          fpf fmt
            (if parameters |> Model.BookParameters.running_header then
               [%blob "template/book/header/one-sided.ly"]
             else
               [%blob "template/book/header/none.ly"]);
          fpf fmt
            (if parameters |> Model.BookParameters.running_footer then
               [%blob "template/book/footer/one-sided.ly"]
             else
               [%blob "template/book/footer/none.ly"])
        );
      fpf fmt [%blob "template/repeat-volta-fancy.ly"];
      fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
      fpf fmt [%blob "template/bar-numbering/bar-number-in-instrument-name-engraver.ly"];
      fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
      fpf fmt [%blob "template/book/book_beginning.ly"];
      if parameters |> Model.BookParameters.front_page then
        fpf fmt [%blob "template/book/book_front_page.ly"];
      if parameters |> Model.BookParameters.table_of_contents |> (=) Model.BookParameters.Beginning then
        fpf fmt [%blob "template/book/book_table_of_contents.ly"];
      let%lwt () =
        let%lwt sets_and_parameters =
          let%lwt contents = Model.Book.contents book in
          Fun.flip Lwt_list.map_p contents @@ function
          | Model.Book.Version (version, parameters) ->
            let%lwt tune = Model.Version.tune version in
            let%lwt name = Model.Tune.name tune in
            let name =
              parameters
              |> Model.VersionParameters.display_name
              |> Option.value ~default:name
            in
            let trivia =
              parameters
              |> Model.VersionParameters.trivia
              |> Option.value ~default:" "
            in
            let%lwt bars = Model.Version.bars version in
            let%lwt kind = Model.Tune.kind tune in
            let parameters = Model.VersionParameters.set_display_name trivia parameters in
            let%lwt set =
              Model.Set.make
                ~name
                ~kind:(Model.Kind.Dance.Version (bars, kind))
                ~versions_and_parameters:[version, parameters]
                ~order:[Internal 1]
                ~modified_at:(NesDatetime.now ())
                ~created_at:(NesDatetime.now ())
                ()
            in
            let%lwt for_dance = Model.VersionParameters.for_dance parameters in
            let%lwt set_parameters = Model.SetParameters.make
                ~display_name:name ?for_dance ~show_order:false () in
            Lwt.return (set, set_parameters)

          | Set (set, parameters) | InlineSet (set, parameters) ->
            Lwt.return (set, parameters)
        in
        Fun.flip Lwt_list.iter_s sets_and_parameters @@ fun (set, set_parameters) ->
        let set_parameters = Model.SetParameters.compose (Model.BookParameters.every_set parameters) set_parameters in
        let%lwt name = Model.Set.name set in
        let name =
          set_parameters
          |> Model.SetParameters.display_name
          |> Option.value ~default:name
        in
        let%lwt deviser =
          if not (set_parameters |> Model.SetParameters.show_deviser) then
            Lwt.return ""
          else
            (match%lwt Model.Set.deviser set with
             | None -> Lwt.return ""
             | Some deviser ->
               let deviser = Model.Person.name deviser in
               Lwt.return (spf "Set by %s" deviser))
        in
        let%lwt kind = kind set set_parameters in
        let%lwt details_line = details_line set set_parameters in
        fpf fmt [%blob "template/book/set_beginning.ly"]
          name kind name deviser details_line;
        (match set_parameters |> Model.SetParameters.forced_pages with
         | 0 -> ()
         | n -> fpf fmt [%blob "template/book/set-forced-pages.ly"] n);
        let%lwt () =
          let%lwt versions_and_parameters = Model.Set.versions_and_parameters set in
          let%lwt order = Model.Set.order set in
          let versions_and_parameters =
            rearrange_set_content
              ~order
              ?order_type:(Model.SetParameters.order_type set_parameters)
              versions_and_parameters
          in
          Fun.flip Lwt_list.iter_s versions_and_parameters @@ fun (version, version_parameters) ->
          let version_parameters = Model.VersionParameters.compose (Model.SetParameters.every_version set_parameters) version_parameters in
          let%lwt content = Model.Version.content version in
          let content =
            match version_parameters |> Model.VersionParameters.clef with
            | None -> content
            | Some clef_parameter ->
              let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
              Str.global_replace clef_regex ("\\clef " ^ Model.Music.clef_to_string clef_parameter) content
          in
          let%lwt tune = Model.Version.tune version in
          let%lwt key = Model.Version.key version in
          let%lwt name = Model.Tune.name tune in
          let name =
            version_parameters
            |> Model.VersionParameters.display_name
            |> Option.value ~default:name
          in
          let%lwt author =
            match%lwt Model.Tune.author tune with
            | None -> Lwt.return ""
            | Some author -> Lwt.return @@ Model.Person.name author
          in
          let author =
            version_parameters
            |> Model.VersionParameters.display_author
            |> Option.value ~default:author
          in
          let first_bar =
            version_parameters
            |> Model.VersionParameters.first_bar
          in
          let source, target =
            match version_parameters |> Model.VersionParameters.transposition with
            | Relative (source, target) -> (source, target)
            | Absolute target -> (key |> Model.Music.key_pitch, target) (* FIXME: probably an octave to fix here *)
          in
          fpf fmt [%blob "template/book/version.ly"]
            name author
            first_bar
            name
            (Model.Music.pitch_to_lilypond_string source)
            (Model.Music.pitch_to_lilypond_string target)
            content;
          Lwt.return ()
        in
        fpf fmt [%blob "template/book/set_end.ly"];
        Lwt.return ()
      in
      if parameters |> Model.BookParameters.table_of_contents |> (=) Model.BookParameters.End then
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
  let cache : ([`Pdf] * Model.Book.t * Model.BookParameters.t option * string, string Lwt.t) StorageCache.t =
    StorageCache.create ()

  let populate_cache () =
    populate_cache ~cache ~ext:".pdf" ~pp_ext:"pdf"

  let render ?parameters book =
    let%lwt body = Model.Book.lilypond_contents_cache_key book in
    StorageCache.use ~cache ~key:(`Pdf, book, parameters, body) @@ fun hash ->
    let%lwt lilypond = Ly.render ?parameters book in
    let path = Filename.concat !Dancelor_server_config.cache "book" in
    let%lwt (fname_ly, fname_pdf) =
      let%lwt slug = Model.Book.slug book in
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
    let%lwt book = Model.Book.get book in
    let%lwt path_pdf = render ?parameters book in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
