(** {1 Conversion from models to renderer}

    This module contains the logic to convert from {!Common.Model} to
    {!Renderer}. It is meant to be used in other controllers. *)

open NesUnix
open Common

module Log = (val Logger.create "controller.modelToRenderer": Logs.LOG)

let format_persons =
  String.concat ", " ~last: " and " %
    List.map (NEString.to_string % Model.Person.name')

let version_to_renderer_tune version version_params =
  let%lwt name = NEString.to_string <$> Model.Version.one_name version in
  let%lwt composer = format_persons <$> (Model.Tune.composers' =<< Model.Version.tune version) in
  (* prepare the content *)
  let content = Model.Version.content version in
  (* update the key *)
  let content =
    match Model.VersionParameters.clef version_params with
    | None -> content
    | Some clef_parameter ->
      let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
      Str.global_replace clef_regex ("\\clef " ^ Music.clef_to_lilypond_string clef_parameter) content
  in
  (* add transposition *)
  let content =
    let key = Model.Version.key version in
    let source, target =
      (* FIXME: Pair.map_both *)
      (fun (x, y) -> (Music.pitch_to_lilypond_string x, Music.pitch_to_lilypond_string y)) @@
        match Model.VersionParameters.transposition' version_params with
        | Relative (source, target) -> (source, target)
        | Absolute target -> (Music.key_pitch key, target)
    in
    spf "\\transpose %s %s { %s }" source target content
  in
  lwt Renderer.{name; composer; content}

let version_to_renderer_tune' version version_params =
  version_to_renderer_tune (Entry.value version) version_params

let part_to_renderer_part name =
  Renderer.{name = NEString.to_string name}

let set_to_renderer_set set set_params =
  let name =
    NEString.to_string @@
      Option.value ~default: (Model.Set.name set) (Model.SetParameters.display_name set_params)
  in
  let%lwt conceptor =
    let%lwt conceptors = Model.Set.conceptors set in
    let default =
      match conceptors with
      | [] -> ""
      | _ -> "Set by " ^ format_persons conceptors
    in
    lwt @@ Option.value ~default (Model.SetParameters.display_conceptor set_params)
  in
  let kind =
    let default = Model.Set.kind set in
    Kind.Dance.to_pretty_string @@
      Option.value ~default (Model.SetParameters.display_kind set_params)
  in
  let every_version_params = Model.SetParameters.every_version set_params in
  let%lwt contents =
    Lwt_list.map_s
      (fun (version, version_params) ->
        let version_params = Model.VersionParameters.compose every_version_params version_params in
        version_to_renderer_tune' version version_params
      )
    =<< Model.Set.contents set
  in
  lwt Renderer.{name; conceptor; kind; contents}

let set_to_renderer_set' set set_params =
  set_to_renderer_set (Entry.value set) set_params

let version_to_renderer_set version version_params set_params =
  let%lwt name =
    let%lwt default = Model.Version.one_name version in
    lwt @@
    NEString.to_string @@
    Option.value ~default (Model.SetParameters.display_name set_params)
  in
  let%lwt conceptor =
    let%lwt default = format_persons <$> (Model.Tune.composers' =<< Model.Version.tune version) in
    lwt @@ Option.value ~default (Model.SetParameters.display_conceptor set_params)
  in
  let%lwt kind =
    let%lwt none = Kind.Version.to_pretty_string <$> Model.Version.kind version in
    lwt @@ Option.fold ~none ~some: Kind.Dance.to_pretty_string (Model.SetParameters.display_kind set_params)
  in
  let%lwt contents = List.singleton <$> version_to_renderer_tune version version_params in
  lwt Renderer.{name; conceptor; kind; contents}

let version_to_renderer_set' version version_params set_params =
  version_to_renderer_set (Entry.value version) version_params set_params

let dance_to_renderer_set set_params =
  set_to_renderer_set
    (
      Model.Set.make
        ~name: (NEString.of_string_exn "should not be seen")
        ~kind: (Version (0, Reel))
        ~order: []
        ()
    )
    set_params

let page_to_renderer_page page book_params =
  let every_set_params = Model.BookParameters.every_set book_params in
  match page with
  | Model.Book.Part title ->
    lwt @@ Renderer.part @@ part_to_renderer_part title
  | Model.Book.Dance (dance, dance_page) ->
    (
      let dance_params =
        let display_name = Model.Dance.one_name' dance in
        let display_kind = Model.Dance.kind' dance in
        Model.SetParameters.make ~display_name ~display_conceptor: "" ~display_kind ()
      in
      let dance_params =
        Model.SetParameters.compose every_set_params dance_params
      in
      match dance_page with
      | DanceOnly ->
        Renderer.set <$> dance_to_renderer_set dance_params
      | DanceVersion (version, version_params) ->
        Renderer.set <$> version_to_renderer_set' version version_params dance_params
      | DanceSet (set, set_params) ->
        let set_params = Model.SetParameters.compose set_params dance_params in
        Renderer.set <$> set_to_renderer_set' set set_params
    )
  | Model.Book.Version (version, version_params) ->
    Renderer.set <$> version_to_renderer_set' version version_params every_set_params
  | Model.Book.Set (set, set_params) ->
    let set_params = Model.SetParameters.compose set_params every_set_params in
    Renderer.set <$> set_to_renderer_set' set set_params

let book_to_renderer_book book book_params =
  let title = NEString.to_string @@ Model.Book.title book in
  let%lwt editor = format_persons <$> Model.Book.authors book in
  let%lwt contents =
    Lwt_list.map_s (fun page -> page_to_renderer_page page book_params)
    =<< Model.Book.contents book
  in
  lwt Renderer.{title; editor; contents}

let book_to_renderer_book' book book_params =
  book_to_renderer_book (Entry.value book) book_params

let grab_renderer_book_pdf_args rendering_params =
  let specificity =
    String.concat ", " @@
      List.flatten
        [
          Option.to_list (RenderingParameters.instruments rendering_params);
          Option.to_list (RenderingParameters.clef rendering_params);
        ]
  in
  let headers = Option.value ~default: true @@ RenderingParameters.show_headers rendering_params in
    (specificity, headers)

let renderer_book_to_renderer_book_pdf_arg book rendering_params =
  let (specificity, headers) = grab_renderer_book_pdf_args rendering_params in
  lwt Renderer.{book; specificity; headers; full = true; two_sided = true}

let renderer_set_to_renderer_book_pdf_arg set rendering_params =
  let title = set.Renderer.name in
  let book = {Renderer.title; editor = ""; contents = [Renderer.Set set]} in
  let (specificity, headers) = grab_renderer_book_pdf_args rendering_params in
  lwt Renderer.{book; specificity; headers; full = false; two_sided = false}
