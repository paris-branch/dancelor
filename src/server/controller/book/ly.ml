open NesUnix
open Common

module Log = (val Logger.create "controller.book.ly": Logs.LOG)

let details_line set set_parameters =
  let kind = Kind.Dance.to_pretty_string @@ Model.Set.kind set in
  let order =
    if Model.SetParameters.show_order' set_parameters then
        [spf "Play %s" @@ Model.SetOrder.to_pretty_string @@ Model.Set.order set]
    else
        []
  in
  lwt (String.concat " — " ([kind] @ order))

(** Rearrange the content of a set. [Default] will leave the content as-is,
    while [Unfolded] will duplicate the tunes depending on the set order. *)
let rearrange_set_content ~order_type ~order content =
  match order_type with
  | Model.SetParameters.Default -> content
  | Model.SetParameters.Unfolded ->
    order
    |> List.map_filter (function Model.SetOrder.Internal n -> Some n | _ -> None)
    |> List.map (List.nth content % flip (-) 1)

let cache : ([`Ly] * Model.Book.t * Model.BookParameters.t * string * RenderingParameters.t, string Lwt.t) StorageCache.t =
  StorageCache.create ()

let render book book_parameters rendering_parameters =
  let%lwt body = Model.Book.lilypond_contents_cache_key book in
  StorageCache.use ~cache ~key: (`Ly, book, book_parameters, body, rendering_parameters) @@ fun _hash ->
  let (res, prom) =
    Format.with_formatter_to_string_gen @@ fun fmt ->
    let title = NEString.to_string @@ Model.Book.title book in
    (** FIXME: subtitle *)
    fpf fmt [%blob "../template/lyversion.ly"];
    (
      match RenderingParameters.paper_size' rendering_parameters with
      | A n -> fpf fmt [%blob "../template/paper_size/a.ly"] n;
      | Custom (width, height, unit) -> fpf fmt [%blob "../template/paper_size/custom.ly"] width unit height unit;
    );
    fpf fmt [%blob "../template/book/macros.ly"];
    fpf fmt [%blob "../template/layout.ly"];
    fpf
      fmt
      [%blob "../template/book/globals.ly"]
      title
      (RenderingParameters.instruments' rendering_parameters);
    fpf
      fmt
      [%blob "../template/book/book_metadata.ly"]
      (RenderingParameters.(title' % pdf_metadata) rendering_parameters)
      (RenderingParameters.(subtitle' % pdf_metadata) rendering_parameters)
      (String.concat "; " @@ RenderingParameters.(composers' % pdf_metadata) rendering_parameters)
      (String.concat "; " @@ RenderingParameters.(subjects' % pdf_metadata) rendering_parameters);
    fpf fmt [%blob "../template/paper.ly"];
    fpf fmt [%blob "../template/book/paper.ly"];
    if Model.BookParameters.two_sided' book_parameters then
      (
        fpf fmt [%blob "../template/book/two_sided.ly"];
        fpf
          fmt
          (
            if Model.BookParameters.running_header' book_parameters then
              [%blob "../template/book/header/two_sided.ly"]
            else
              [%blob "../template/book/header/none.ly"]
          );
        fpf
          fmt
          (
            if Model.BookParameters.running_footer' book_parameters then
              [%blob "../template/book/footer/two_sided.ly"]
            else
              [%blob "../template/book/footer/none.ly"]
          )
      )
    else
      (
        fpf
          fmt
          (
            if Model.BookParameters.running_header' book_parameters then
              [%blob "../template/book/header/one_sided.ly"]
            else
              [%blob "../template/book/header/none.ly"]
          );
        fpf
          fmt
          (
            if Model.BookParameters.running_footer' book_parameters then
              [%blob "../template/book/footer/one_sided.ly"]
            else
              [%blob "../template/book/footer/none.ly"]
          )
      );
    fpf fmt [%blob "../template/helpers.ly"];
    fpf fmt [%blob "../template/repeat_volta_fancy.ly"];
    fpf fmt [%blob "../template/bar_numbering/repeat_aware.ly"];
    fpf fmt [%blob "../template/bar_numbering/bar_number_in_instrument_name_engraver.ly"];
    fpf fmt [%blob "../template/bar_numbering/beginning_of_line.ly"];
    fpf fmt [%blob "../template/book/book_beginning.ly"];
    if Model.BookParameters.front_page' book_parameters then
      fpf fmt [%blob "../template/book/book_front_page.ly"];
    if Model.BookParameters.(table_of_contents' book_parameters = Beginning) then
      fpf fmt [%blob "../template/book/book_table_of_contents.ly"];
    let%lwt () =
      let%lwt sets_and_parameters =
        let%lwt contents = Model.Book.contents book in
        flip Lwt_list.map_p contents @@ function
          | Model.Book.Part title ->
            lwt (
              Model.Set.make ~name: title ~kind: (Kind.Dance.Version (0, Kind.Base.Strathspey)) ~order: [] (),
              Model.SetParameters.none
            )
          | Model.Book.Dance (dance, DanceOnly) ->
            let name = Model.Dance.one_name' dance in
            lwt (
              Model.Set.make ~name ~kind: (Kind.Dance.Version (0, Kind.Base.Strathspey)) ~order: [] (),
              Model.SetParameters.none
            )
          | Model.Book.Dance (_, DanceVersion (version, parameters))
          | Model.Book.Version (version, parameters) ->
            let%lwt tune = Model.Version.tune' version in
            let name = Option.value ~default: (Model.Tune.one_name' tune) (Model.VersionParameters.display_name parameters) in
            let set =
              Model.Set.make
                ~name
                ~kind: (Kind.Dance.Version (Model.Version.bars' version, Model.Tune.kind' tune))
                ~contents: [version, parameters]
                ~order: [Internal 1]
                ()
            in
            let set_parameters = Model.SetParameters.make ~display_name: name ~show_order: false () in
            lwt (set, set_parameters)
          | Model.Book.Dance (_, DanceSet (set, parameters))
          | Model.Book.Set (set, parameters) ->
            lwt (Entry.value set, parameters)
      in
      (* FIXME: none of the above need to be dummy; I think we can just return
         a SetCore.t; do we need the id anyway? *)
      flip Lwt_list.iter_s sets_and_parameters @@ fun (set, set_parameters) ->
      let set_parameters = Model.SetParameters.compose (Model.BookParameters.every_set book_parameters) set_parameters in
      let name = Option.value ~default: (Model.Set.name set) (Model.SetParameters.display_name set_parameters) in
      let%lwt deviser =
        if not (Model.SetParameters.show_deviser' set_parameters) then
          lwt_empty
        else
          match%lwt Model.Set.conceptors set with
          | [] -> lwt_empty
          | devisers -> lwt ("Set by " ^ String.concat ", " ~last: " & " @@ List.map (NEString.to_string % Model.Person.name') devisers)
      in
      let kind = Kind.Dance.to_pretty_string @@ Model.Set.kind set in
      let%lwt details_line = details_line set set_parameters in
      fpf
        fmt
        [%blob "../template/book/set_beginning.ly"]
        (NEString.to_string name)
        kind
        (NEString.to_string name)
        deviser
        details_line;
      (
        match Model.SetParameters.forced_pages' set_parameters with
        | 0 -> ()
        | n -> fpf fmt [%blob "../template/book/set_forced_pages.ly"] n
      );
      let%lwt () =
        let%lwt contents = Model.Set.contents set in
        let contents =
          rearrange_set_content
            ~order: (Model.Set.order set)
            ~order_type: (Model.SetParameters.order_type' set_parameters)
            contents
        in
        flip Lwt_list.iter_s contents @@ fun (version, version_parameters) ->
        let version_parameters = Model.VersionParameters.compose (Model.SetParameters.every_version set_parameters) version_parameters in
        let content =
          let content = Model.Version.content' version in
          match Model.VersionParameters.clef version_parameters with
          | None -> content
          | Some clef_parameter ->
            let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
            Str.global_replace clef_regex ("\\clef " ^ Music.clef_to_string clef_parameter) content
        in
        let%lwt tune = Model.Version.tune' version in
        let key = Model.Version.key' version in
        let name = Option.value ~default: (Model.Tune.one_name' tune) (Model.VersionParameters.display_name version_parameters) in
        let%lwt composer = (String.concat ", " ~last: " and " % List.map (NEString.to_string % Model.Person.name')) <$> Model.Tune.composers' tune in
        let composer = Option.fold ~none: composer ~some: NEString.to_string (Model.VersionParameters.display_composer version_parameters) in
        let first_bar = Model.VersionParameters.first_bar' version_parameters in
        let source, target =
          match Model.VersionParameters.transposition' version_parameters with
          | Relative (source, target) -> (source, target)
          | Absolute target -> (Music.key_pitch key, target) (* FIXME: probably an octave to fix here *)
        in fpf
          fmt
          [%blob "../template/book/version.ly"]
          (NEString.to_string name)
          composer
          first_bar
          (NEString.to_string name)
          (Music.pitch_to_lilypond_string source)
          (Music.pitch_to_lilypond_string target)
          content;
        lwt_unit
      in
      fpf fmt [%blob "../template/book/set_end.ly"];
      lwt_unit
    in
    if Model.BookParameters.(table_of_contents' book_parameters = End) then
      fpf fmt [%blob "../template/book/book_table_of_contents.ly"];
    fpf fmt [%blob "../template/book/book_end.ly"];
    lwt_unit
  in
  prom;%lwt
  lwt res
