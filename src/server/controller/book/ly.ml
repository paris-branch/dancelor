open NesUnix
open Common

module Log = (val Logger.create "controller.book.ly": Logs.LOG)

let kind set set_parmeters =
  let%lwt kind =
    match%lwt Model.SetParameters.for_dance set_parmeters with
    | None -> lwt @@ Model.Set.kind set
    | Some dance -> lwt @@ Model.Dance.kind' dance
  in
  lwt (Kind.Dance.to_pretty_string kind)

let details_line set set_parameters =
  let%lwt dance =
    match%lwt Model.SetParameters.for_dance set_parameters with
    | None -> lwt_nil
    | Some dance -> lwt [spf "Dance: %s" (Model.Dance.name' dance)]
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
    | Some dance when Model.Dance.two_chords' dance = Some true -> lwt ["Two Chords"]
    | _ -> lwt_nil
  in
  lwt (String.concat " — " (dance @ [kind] @ order @ chords))

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
    let title = Model.Book.title book in
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
      (Model.BookParameters.instruments' book_parameters);
    fpf
      fmt
      [%blob "../template/book/book_metadata.ly"]
      (RenderingParameters.(title % pdf_metadata) rendering_parameters)
      (RenderingParameters.(subtitle % pdf_metadata) rendering_parameters)
      (String.concat "; " @@ RenderingParameters.(composers % pdf_metadata) rendering_parameters)
      (String.concat "; " @@ RenderingParameters.(subjects % pdf_metadata) rendering_parameters);
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
          | Model.Book.Version (version, parameters) ->
            let%lwt tune = Model.Version.tune' version in
            let name = Model.VersionParameters.display_name' ~default: (Model.Tune.name' tune) parameters in
            let trivia = Model.VersionParameters.trivia' ~default: " " parameters in
            let parameters = Model.VersionParameters.set_display_name trivia parameters in
            let set =
              Model.Set.make
                ~name
                ~kind: (Kind.Dance.Version (Model.Version.bars' version, Model.Tune.kind' tune))
                ~contents: [version, parameters]
                ~order: [Internal 1]
                ()
            in
            let%lwt for_dance = Model.VersionParameters.for_dance parameters in
            let%lwt set_parameters =
              Model.SetParameters.make'
                ~display_name: name
                ?for_dance
                ~show_order: false
                ()
            in
            lwt (set, set_parameters)
          | Set (set, parameters) -> lwt (Entry.value set, parameters)
          | InlineSet (set, parameters) -> lwt (set, parameters)
      in
      (* FIXME: none of the above need to be dummy; I think we can just return
         a SetCore.t; do we need the id anyway? *)
      flip Lwt_list.iter_s sets_and_parameters @@ fun (set, set_parameters) ->
      let set_parameters = Model.SetParameters.compose (Model.BookParameters.every_set book_parameters) set_parameters in
      let name = Model.SetParameters.display_name' ~default: (Model.Set.name set) set_parameters in
      let%lwt deviser =
        if not (Model.SetParameters.show_deviser' set_parameters) then
          lwt_empty
        else
          match%lwt Model.Set.conceptors set with
          | [] -> lwt_empty
          | devisers -> lwt ("Set by " ^ String.concat ", " ~last: " & " @@ List.map Model.Person.name' devisers)
      in
      let%lwt kind = kind set set_parameters in
      let%lwt details_line = details_line set set_parameters in
      fpf
        fmt
        [%blob "../template/book/set_beginning.ly"]
        name
        kind
        name
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
        let name = Model.VersionParameters.display_name' ~default: (Model.Tune.name' tune) version_parameters in
        let%lwt composer = (String.concat ", " ~last: " and " % List.map Model.Person.name') <$> Model.Tune.composers' tune in
        let composer = Model.VersionParameters.display_composer' ~default: composer version_parameters in
        let first_bar = Model.VersionParameters.first_bar' version_parameters in
        let source, target =
          match Model.VersionParameters.transposition' version_parameters with
          | Relative (source, target) -> (source, target)
          | Absolute target -> (Music.key_pitch key, target) (* FIXME: probably an octave to fix here *)
        in fpf
          fmt
          [%blob "../template/book/version.ly"]
          name
          composer
          first_bar
          name
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
