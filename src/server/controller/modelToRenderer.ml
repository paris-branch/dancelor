(** {1 Conversion from models to renderer}

    This module contains the logic to convert from {!Common.Model} to
    {!Renderer}. It is meant to be used in other controllers. *)

open NesUnix
open Common

module Log = (val Logger.create "controller.modelToRenderer": Logs.LOG)

let format_persons_list =
  List.map (NEString.to_string % Model.Person.name')

let format_persons =
  String.concat ", " ~last: " and " % format_persons_list

let version_to_lilypond_content ~version_params version =
  (* get a LilyPond from the potentially-destructured content *)
  let structure = Model.VersionParameters.structure version_params in
  let%lwt content = Model.Version.content_lilypond ?structure version in
  let instructions =
    (* if the version is destructured, and the user asked for a structure, but
       we could not find a fold for this structure, then at least we produce the
       instruction to play that structure as we generate a destructured output *)
    match Model.Version.content version with
    | Monolithic _ -> None
    | Destructured _ ->
      match structure with
      | None -> None
      | Some structure ->
        match Model.Version.Structure.best_fold_for structure with
        | Some _ -> None
        | None -> Some ("Play " ^ NEString.to_string (Model.Version.Structure.to_string structure))
  in
  (* update the clef *)
  let content =
    match Model.VersionParameters.clef version_params with
    | None -> content
    | Some clef_parameter ->
      let clef_regex = Str.regexp "\\\\clef *\"?[a-z]*\"?" in
      Str.global_replace clef_regex ("\\clef " ^ Music.Clef.to_string clef_parameter) content
  in
  (* add transposition *)
  let content =
    let source = Music.Key.pitch @@ Model.Version.key version in
    let target = Transposition.target_pitch ~source @@ Option.value ~default: Transposition.identity @@ Model.VersionParameters.transposition version_params in
    let (source, target) = Pair.map_both Music.Pitch.to_lilypond_string (source, target) in
    spf "\\transpose %s %s { %s }" source target content
  in
  (* done *)
  lwt (content, instructions)

let version_to_renderer_tune ?(version_params = Model.VersionParameters.none) version =
  let%lwt slug = NesSlug.to_string <$> Model.Version.slug version in
  let%lwt name =
    let%lwt default = Model.Version.one_name version in
    lwt @@
    NEString.to_string @@
    Option.value
      ~default
      (Model.VersionParameters.display_name version_params)
  in
  let%lwt composer =
    let%lwt none = format_persons <$> (Model.Tune.composers' =<< Model.Version.tune version) in
    lwt @@
      Option.fold
        ~none
        ~some: NEString.to_string
        (Model.VersionParameters.display_composer version_params)
  in
  let%lwt (content, instructions) = version_to_lilypond_content ~version_params version in
  let instructions = Option.value instructions ~default: "" in
  let first_bar = Model.VersionParameters.first_bar' version_params in
  let%lwt tune = Model.Version.tune version in
  let kind = Model.Tune.kind' tune in
  let (tempo_unit, tempo_value) = Kind.Base.tempo kind in
  let chords_kind = Kind.Base.to_pretty_string ~capitalised: false kind in
  let show_bar_numbers =
    Model.Version.(Content.is_monolithic @@ content version)
    || Model.VersionParameters.structure version_params <> None
  in
  lwt Renderer.{slug; name; instructions; composer; content; first_bar; tempo_unit; tempo_value; chords_kind; show_bar_numbers}

let version_to_renderer_tune' ?version_params version =
  version_to_renderer_tune ?version_params (Entry.value version)

let part_to_renderer_part name =
  Renderer.{name = NEString.to_string name}

let set_to_renderer_set set set_params =
  let slug = NesSlug.to_string @@ Model.Set.slug set in
  let name =
    NEString.to_string @@
      Option.value ~default: (Model.Set.name set) (Model.SetParameters.display_name set_params)
  in
  let%lwt conceptor =
    let%lwt conceptors = Model.Set.conceptors set in
    let none =
      match conceptors with
      | [] -> ""
      | _ -> "Set by " ^ format_persons conceptors
    in
    lwt @@ Option.fold ~none ~some: NEString.to_string (Model.SetParameters.display_conceptor set_params)
  in
  let kind =
    let none = Kind.Dance.to_pretty_string @@ Model.Set.kind set in
    Option.fold ~none ~some: NEString.to_string (Model.SetParameters.display_kind set_params)
  in
  let every_version_params = Model.SetParameters.every_version set_params in
  let%lwt contents =
    Lwt_list.map_s
      (fun (version, version_params) ->
        let version_params = Model.VersionParameters.compose every_version_params version_params in
        version_to_renderer_tune' version ~version_params
      )
    =<< Model.Set.contents set
  in
  lwt Renderer.{slug; name; conceptor; kind; contents}

let set_to_renderer_set' set set_params =
  set_to_renderer_set (Entry.value set) set_params

let versions_to_renderer_set versions_and_params set_params =
  let%lwt name =
    let%lwt name =
      String.concat ", " ~last: " and "
      <$> Lwt_list.map_s (NEString.to_string <%> Model.Version.one_name % fst) (NEList.to_list versions_and_params)
    in
    lwt @@ Option.fold ~none: name ~some: NEString.to_string (Model.SetParameters.display_name set_params)
  in
  let slug = NesSlug.(to_string % of_string) name in
  let conceptor =
    Option.fold ~none: "" ~some: NEString.to_string (Model.SetParameters.display_conceptor set_params)
  in
  let kind = "" in
  let%lwt contents =
    Lwt_list.map_s (fun (version, version_params) -> version_to_renderer_tune version ~version_params) (NEList.to_list versions_and_params)
  in
  lwt Renderer.{slug; name; conceptor; kind; contents}

let versions_to_renderer_set' versions_and_params set_params =
  versions_to_renderer_set (NEList.map (Pair.map_fst Entry.value) versions_and_params) set_params

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
      let%lwt dance_params =
        let display_name = Model.Dance.one_name' dance in
        let%lwt display_conceptor =
          let%lwt devisers = Model.Dance.devisers' dance in
          lwt @@
          NEString.of_string_exn @@
          match devisers with
          | [] -> " "
          | _ -> "Dance by " ^ format_persons devisers
        in
        let display_kind =
          NEString.of_string_exn @@
          (Kind.Dance.to_pretty_string @@ Model.Dance.kind' dance) ^ (
            match Model.Dance.two_chords' dance with
            | None -> " — Two chords: unknown"
            | Some true -> " — Two chords"
            | Some false -> ""
          )
        in
        lwt @@ Model.SetParameters.make ~display_name ~display_conceptor ~display_kind ()
      in
      let dance_params =
        Model.SetParameters.compose every_set_params dance_params
      in
      match dance_page with
      | DanceOnly ->
        Renderer.set <$> dance_to_renderer_set dance_params
      | DanceVersions versions_and_params ->
        Renderer.set <$> versions_to_renderer_set' versions_and_params dance_params
      | DanceSet (set, set_params) ->
        let set_params = Model.SetParameters.compose set_params dance_params in
        Renderer.set <$> set_to_renderer_set' set set_params
    )
  | Model.Book.Versions versions_and_params ->
    Renderer.set <$> versions_to_renderer_set' versions_and_params every_set_params
  | Model.Book.Set (set, set_params) ->
    let set_params = Model.SetParameters.compose set_params every_set_params in
    Renderer.set <$> set_to_renderer_set' set set_params

let book_to_renderer_book book book_params =
  let slug = NesSlug.to_string @@ Model.Book.slug book in
  let title = NEString.to_string @@ Model.Book.title book in
  let%lwt editor = format_persons <$> Model.Book.authors book in
  let%lwt contents =
    Lwt_list.map_s (fun page -> page_to_renderer_page page book_params)
    =<< Model.Book.contents book
  in
  let simple = Option.value ~default: false @@ Model.BookParameters.simple book_params in
  lwt Renderer.{slug; title; editor; contents; simple}

let book_to_renderer_book' book book_params =
  book_to_renderer_book (Entry.value book) book_params

let grab_renderer_book_pdf_args rendering_params =
  let specificity =
    String.concat ", " ~last: " and " @@
      List.flatten
        [
          Option.to_list (Rendering_parameters.instruments rendering_params);
          Option.to_list (Rendering_parameters.clef rendering_params);
        ]
  in
  let headers = Option.value ~default: true @@ Rendering_parameters.show_headers rendering_params in
    (specificity, headers)

let renderer_book_to_renderer_book_pdf_arg (book : Renderer.book) rendering_params pdf_metadata =
  let (specificity, headers) = grab_renderer_book_pdf_args rendering_params in
  lwt Renderer.{book; specificity; headers; pdf_metadata}

let renderer_set_to_renderer_book_pdf_arg (set : Renderer.set) rendering_params pdf_metadata =
  let slug = set.Renderer.slug in
  let title = set.Renderer.name in
  let book = {Renderer.slug; title; editor = ""; contents = [Renderer.Set set]; simple = true} in
  let (specificity, headers) = grab_renderer_book_pdf_args rendering_params in
  lwt Renderer.{book; specificity; headers; pdf_metadata}
