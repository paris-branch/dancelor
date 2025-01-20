open NesUnix
module Model = Dancelor_server_model
module Database = Dancelor_server_database
module Log = (val Dancelor_server_logs.create "controller.book": Logs.LOG)

module Ly = struct
  let kind set set_parmeters =
    let%lwt kind =
      match%lwt Model.SetParameters.for_dance set_parmeters with
      | None -> Lwt.return @@ Model.Set.kind set
      | Some dance -> Lwt.return @@ Model.Dance.kind dance
    in
    Lwt.return (Model.Kind.Dance.to_pretty_string kind)

  let details_line set set_parameters =
    let%lwt dance =
      match%lwt Model.SetParameters.for_dance set_parameters with
      | None -> Lwt.return_nil
      | Some dance -> Lwt.return [spf "Dance: %s" (Model.Dance.name dance)]
    in
    let%lwt kind = kind set set_parameters in
    let order =
      if Model.SetParameters.show_order' set_parameters then
        [spf "Play %s" @@ Model.SetOrder.to_pretty_string @@ Model.Set.order set]
      else
        []
    in
    let%lwt chords =
      match%lwt Model.SetParameters.for_dance set_parameters with
      | Some dance when Model.Dance.two_chords dance = Some true -> Lwt.return ["Two Chords"]
      | _ -> Lwt.return_nil
    in
    Lwt.return (String.concat " — " (dance @ [kind] @ order @ chords))

  (** Rearrange the content of a set. [Default] will leave the content as-is,
      while [Unfolded] will duplicate the tunes depending on the set order. *)
  let rearrange_set_content ~order_type ~order content =
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

  let render ?(parameters = Model.BookParameters.none) book =
    let%lwt body = Model.Book.lilypond_contents_cache_key book in
    StorageCache.use ~cache ~key: (`Ly, book, parameters, body) @@ fun _hash ->
    let (res, prom) =
      Format.with_formatter_to_string_gen @@ fun fmt ->
      let title = Model.Book.title book in (** FIXME: subtitle *)
      fpf fmt [%blob "template/lyversion.ly"];
      (
        match Model.BookParameters.paper_size' parameters with
        | A n -> fpf fmt [%blob "template/paper-size/a.ly"] n;
        | Custom (width, height, unit) -> fpf fmt [%blob "template/paper-size/custom.ly"] width unit height unit;
      );
      fpf fmt [%blob "template/book/macros.ly"];
      fpf fmt [%blob "template/layout.ly"];
      fpf
        fmt
        [%blob "template/book/globals.ly"]
        title
        (Model.BookParameters.instruments' parameters);
      fpf fmt [%blob "template/paper.ly"];
      fpf fmt [%blob "template/book/paper.ly"];
      if Model.BookParameters.two_sided' parameters then
        (
          fpf fmt [%blob "template/book/two-sided.ly"];
          fpf
            fmt
            (
              if Model.BookParameters.running_header' parameters then
                [%blob "template/book/header/two-sided.ly"]
              else
                [%blob "template/book/header/none.ly"]
            );
          fpf
            fmt
            (
              if Model.BookParameters.running_footer' parameters then
                [%blob "template/book/footer/two-sided.ly"]
              else
                [%blob "template/book/footer/none.ly"]
            )
        )
      else
        (
          fpf
            fmt
            (
              if Model.BookParameters.running_header' parameters then
                [%blob "template/book/header/one-sided.ly"]
              else
                [%blob "template/book/header/none.ly"]
            );
          fpf
            fmt
            (
              if Model.BookParameters.running_footer' parameters then
                [%blob "template/book/footer/one-sided.ly"]
              else
                [%blob "template/book/footer/none.ly"]
            )
        );
      fpf fmt [%blob "template/repeat-volta-fancy.ly"];
      fpf fmt [%blob "template/bar-numbering/repeat-aware.ly"];
      fpf fmt [%blob "template/bar-numbering/bar-number-in-instrument-name-engraver.ly"];
      fpf fmt [%blob "template/bar-numbering/beginning-of-line.ly"];
      fpf fmt [%blob "template/book/book_beginning.ly"];
      if Model.BookParameters.front_page' parameters then
        fpf fmt [%blob "template/book/book_front_page.ly"];
      if Model.BookParameters.(table_of_contents' parameters = Beginning) then
        fpf fmt [%blob "template/book/book_table_of_contents.ly"];
      let%lwt () =
        let%lwt sets_and_parameters =
          let%lwt contents = Model.Book.contents book in
          Fun.flip Lwt_list.map_p contents @@ function
          | Model.Book.Version (version, parameters) ->
            let%lwt tune = Model.Version.tune version in
            let name = Model.VersionParameters.display_name' ~default: (Model.Tune.name tune) parameters in
            let trivia = Model.VersionParameters.trivia' ~default: " " parameters in
            let parameters = Model.VersionParameters.set_display_name trivia parameters in
            let%lwt set =
              Model.Set.make
                ~name
                ~kind: (Model.Kind.Dance.Version (Model.Version.bars version, Model.Tune.kind tune))
                ~contents: [version, parameters]
                ~order: [Internal 1]
                ~modified_at: (NesDatetime.now ())
                ~created_at: (NesDatetime.now ())
                ()
            in
            let%lwt for_dance = Model.VersionParameters.for_dance parameters in
            let%lwt set_parameters =
              Model.SetParameters.make
                ~display_name: name
                ?for_dance
                ~show_order: false
                ()
            in
            Lwt.return (set, set_parameters)
          | Set (set, parameters) | InlineSet (set, parameters) ->
            Lwt.return (set, parameters)
        in
        Fun.flip Lwt_list.iter_s sets_and_parameters @@ fun (set, set_parameters) ->
        let set_parameters = Model.SetParameters.compose (Model.BookParameters.every_set parameters) set_parameters in
        let name = Model.SetParameters.display_name' ~default: (Model.Set.name set) set_parameters in
        let%lwt deviser =
          if not (Model.SetParameters.show_deviser' set_parameters) then
            Lwt.return ""
          else
            match%lwt Model.Set.conceptors set with
            | [] -> Lwt.return ""
            | devisers -> Lwt.return ("Set by " ^ String.concat ", " ~last: " & " @@ List.map Model.Person.name devisers)
        in
        let%lwt kind = kind set set_parameters in
        let%lwt details_line = details_line set set_parameters in
        fpf
          fmt
          [%blob "template/book/set_beginning.ly"]
          name
          kind
          name
          deviser
          details_line;
        (
          match Model.SetParameters.forced_pages' set_parameters with
          | 0 -> ()
          | n -> fpf fmt [%blob "template/book/set-forced-pages.ly"] n
        );
        let%lwt () =
          let%lwt contents = Model.Set.contents set in
          let contents =
            rearrange_set_content
              ~order: (Model.Set.order set)
              ~order_type: (Model.SetParameters.order_type' set_parameters)
              contents
          in
          Fun.flip Lwt_list.iter_s contents @@ fun (version, version_parameters) ->
          let version_parameters = Model.VersionParameters.compose (Model.SetParameters.every_version set_parameters) version_parameters in
          let%lwt content = Model.Version.content version in
          let content =
            match Model.VersionParameters.clef version_parameters with
            | None -> content
            | Some clef_parameter ->
              let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
              Str.global_replace clef_regex ("\\clef " ^ Model.Music.clef_to_string clef_parameter) content
          in
          let%lwt tune = Model.Version.tune version in
          let key = Model.Version.key version in
          let name = Model.VersionParameters.display_name' ~default: (Model.Tune.name tune) version_parameters in
          let%lwt composer = Lwt.map (String.concat ", " ~last: " and " % List.map Model.Person.name) (Model.Tune.composers tune) in
          let composer = Model.VersionParameters.display_composer' ~default: composer version_parameters in
          let first_bar = Model.VersionParameters.first_bar' version_parameters in
          let source, target =
            match Model.VersionParameters.transposition' version_parameters with
            | Relative (source, target) -> (source, target)
            | Absolute target -> (Model.Music.key_pitch key, target) (* FIXME: probably an octave to fix here *)
          in
          fpf
            fmt
            [%blob "template/book/version.ly"]
            name
            composer
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
      if Model.BookParameters.(table_of_contents' parameters = End) then
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

module Pdf = struct
  let cache : ([`Pdf] * Model.Book.t * Model.BookParameters.t option * string, string Lwt.t) StorageCache.t =
    StorageCache.create ()

  let populate_cache () =
    populate_cache ~cache ~ext: ".pdf" ~pp_ext: "pdf"

  let render ?parameters book =
    let%lwt body = Model.Book.lilypond_contents_cache_key book in
    StorageCache.use ~cache ~key: (`Pdf, book, parameters, body) @@ fun hash ->
    let%lwt lilypond = Ly.render ?parameters book in
    let path = Filename.concat !Dancelor_server_config.cache "book" in
    let%lwt (fname_ly, fname_pdf) =
      let slug = Database.Entry.slug book in
      let fname = aspf "%a-%a" Slug.pp' slug StorageCache.pp_hash hash in
      Lwt.return (fname ^ ".ly", fname ^ ".pdf")
    in
    Lwt_io.with_file
      ~mode: Output
      (Filename.concat path fname_ly)
      (fun ochan -> Lwt_io.write ochan lilypond);%lwt
    Log.debug (fun m -> m "Processing with LilyPond");
    LilyPond.run
      ~exec_path: path
      ~fontconfig_file: (Filename.concat !Dancelor_server_config.share "fonts.conf")
      fname_ly;%lwt
    let path_pdf = Filename.concat path fname_pdf in
    Lwt.return path_pdf

  let get book parameters =
    let%lwt book = Model.Book.get book in
    let%lwt path_pdf = render ?parameters book in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
end

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model.BookEndpoints.t -> a = function
  | Get -> Model.Book.get
  | Search -> (fun slice threshold filter -> Model.Book.search ?slice ?threshold filter)
  | Save ->
    (fun status title date contents modified_at created_at ->
       let%lwt contents =
         match contents with
         | None -> Lwt.return_none
         | Some contents ->
           let%lwt contents = Lwt_list.map_s Model.Book.page_core_to_page contents in
           Lwt.return_some contents
       in
       Model.Book.save ?status ~title ?date ?contents ~modified_at ~created_at ()
    )
  | Update ->
    (fun status title date contents modified_at created_at slug ->
       let%lwt contents =
         match contents with
         | None -> Lwt.return_none
         | Some contents ->
           let%lwt contents = Lwt_list.map_s Model.Book.page_core_to_page contents in
           Lwt.return_some contents
       in
       Model.Book.update ?status ~slug ~title ?date ?contents ~modified_at ~created_at ()
    )
  | Pdf -> (fun parameters book -> Pdf.get book parameters)
