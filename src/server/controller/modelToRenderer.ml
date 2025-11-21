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

(** Structures, but with more structure *)
type structure_item = Part of Model.Version.Content.Part_name.t | Repeat of int * structure
and structure = structure_item list

(** A few helpers *)
let a = Part (Model.Version.Content.Part_name.of_char_exn 'A')
let b = Part (Model.Version.Content.Part_name.of_char_exn 'B')
let c = Part (Model.Version.Content.Part_name.of_char_exn 'C')
let d = Part (Model.Version.Content.Part_name.of_char_exn 'D')
let e = Part (Model.Version.Content.Part_name.of_char_exn 'E')
let f = Part (Model.Version.Content.Part_name.of_char_exn 'F')
let repeat n x = Repeat (n, x)

let rec first_part_exn structure =
  match List.hd_opt structure with
  | None -> failwith "first_part_exn"
  | Some Part a -> a
  | Some Repeat (_, structure) -> first_part_exn structure

let first_part structure =
  match List.hd_opt structure with
  | None -> None
  | Some Part a -> Some a
  | Some Repeat (_, structure) -> Some (first_part_exn structure)

let rec last_part_exn structure =
  match List.ft_opt structure with
  | None -> failwith "last_part_exn"
  | Some Part a -> a
  | Some Repeat (_, structure) -> last_part_exn structure

let last_part structure =
  match List.ft_opt structure with
  | None -> None
  | Some Part a -> Some a
  | Some Repeat (_, structure) -> Some (last_part_exn structure)

let ends_with_repeat structure =
  match List.ft structure with
  | Repeat _ -> true
  | _ -> false

(** A general algorithm would be great, but for now this will do. *)
let best_structure_for = function
  | "A" -> some [a]
  | "B" -> some [b]
  | "C" -> some [c]
  | "D" -> some [d]
  | "AB" -> some [a; b]
  | "AAB" -> some [repeat 2 [a]; b]
  | "ABB" -> some [a; repeat 2 [b]]
  | "ABC" -> some [a; b; c]
  | "AAAA" -> some [repeat 4 [a]]
  | "AAAB" -> some [repeat 3 [a]; b]
  | "AABA" -> some [repeat 2 [a]; b; a]
  | "AABB" -> some [repeat 2 [a]; repeat 2 [b]]
  | "ABAA" -> some [a; b; repeat 2 [a]]
  | "ABAB" -> some [repeat 2 [a; b]]
  | "ABBA" -> some [a; repeat 2 [b]; a]
  | "ABBB" -> some [a; repeat 3 [b]]
  | "AABC" -> some [repeat 2 [a]; b; c]
  | "ABCD" -> some [a; b; c; d]
  | "AABBB" -> some [repeat 2 [a]; repeat 3 [b]]
  | "ABABA" -> some [repeat 2 [a; b]; a]
  | "ABABB" -> some [repeat 2 [a; b]; b]
  | "ABBAB" -> some [a; repeat 2 [b]; a; b]
  | "ABBCC" -> some [a; repeat 2 [b]; repeat 2 [c]]
  | "ABABC" -> some [repeat 2 [a; b]; c]
  | "ABCDE" -> some [a; b; c; d; e]
  | "ABABAB" -> some [repeat 3 [a; b]]
  | "AABBAB" -> some [repeat 2 [a]; repeat 2 [b]; a; b]
  | "AABBCC" -> some [repeat 2 [a]; repeat 2 [b]; repeat 2 [c]]
  | "ABCDEF" -> some [a; b; c; d; e; f]
  | "AABBCCDD" -> some [repeat 2 [a]; repeat 2 [b]; repeat 2 [c]; repeat 2 [d]]
  | "AABCDDEF" -> some [repeat 2 [a]; b; c; repeat 2 [d]; e; f]
  | _ -> None

let version_parts_to_lilypond_content ~version_params version parts transitions =
  ignore version_params;
  let transitions =
    (* rearrange the given transitions because we will want to List.assoc later *)
    List.map (fun (p1, p2, t) -> ((p1, p2), t)) transitions
  in
  let%lwt kind = Model.Version.kind version in
  let key = Model.Version.key version in
  let time =
    match kind with
    | Kind.Base.Reel -> "2/2"
    | Jig -> "6/8"
    | Strathspey -> "4/4"
    | Waltz -> "3/4"
    | Polka -> "2/2"
  in
  let key =
    (Music.Note.to_lilypond_string @@ Music.Pitch.note @@ Music.Key.pitch key) ^
    " " ^ (Music.Mode.to_lilypond_string @@ Music.Key.mode key)
  in
  let parts = NEList.to_list parts in
  let (melody, chords, instructions) =
    let desired_structure = Option.map (NEString.to_string % Model.Version.Content.structure_to_string) (Model.VersionParameters.structure version_params) in
    let structure = Option.fold ~none: None ~some: best_structure_for desired_structure in
    match structure with
    | None ->
      (* no structure; or we couldn't find a good one *)
      let melody =
        String.concat " \\section\\break " (
          List.mapi
            (fun part_name part ->
              spf "\\mark\\markup\\box{%c} %s" (Model.Version.Content.Part_name.to_char part_name) part.Model.Version.Content.melody
            )
            parts
        ) ^
          "\\fine"
      in
      let chords =
        String.concat " " (
          List.map (fun part -> part.Model.Version.Content.chords) parts
        )
      in
      let instructions = Option.map (fun structure -> "play " ^ structure) desired_structure in
        (melody, chords, instructions)
    | Some structure ->
      let rec item_to_lilypond ~next_part = function
        | Part part ->
          (
            let lilypond = List.nth parts part in
            match List.assoc_opt (Model.Version.Content.Part_name.Middle part, next_part) transitions with
            | None -> lilypond
            | Some transition ->
              {
                melody = lilypond.melody ^ " " ^ transition.Model.Version.Content.melody;
                chords = lilypond.chords ^ " " ^ transition.Model.Version.Content.chords;
              }
          )
        | Repeat (times, structure) ->
          (
            let lilypond : Model.Version.Content.part = to_lilypond structure in
            let empty_lilypond : Model.Version.Content.part = {melody = ""; chords = ""} in
            let first_part = first_part_exn structure in
            let last_part = last_part_exn structure in
            let middle = Model.Version.Content.Part_name.middle in
            let alt_1 = Option.value ~default: empty_lilypond @@ List.assoc_opt (middle last_part, middle first_part) transitions in
            let alt_2 = Option.value ~default: empty_lilypond @@ List.assoc_opt (middle last_part, next_part) transitions in
            {
              Model.Version.Content.melody =
              spf
                "\\repeat volta %d { %s } \\alternative { { %s } { %s } }"
                times
                lilypond.melody
                alt_1.melody
                alt_2.melody;
              chords =
              spf
                "%s %s %s"
                lilypond.chords
                alt_1.chords
                alt_2.chords;
            }
          )
      and map_item_to_lilypond = function
        | [] -> []
        | item :: items ->
          let next_part = Option.fold ~none: Model.Version.Content.Part_name.End ~some: Model.Version.Content.Part_name.middle (first_part items) in
          item_to_lilypond ~next_part item :: map_item_to_lilypond items
      and to_lilypond structure =
        let structure = map_item_to_lilypond structure in
        {
          melody = String.concat " \\section\\break " (List.map Model.Version.Content.melody structure);
          chords = String.concat " " (List.map Model.Version.Content.chords structure);
        }
      in
      let Model.Version.Content.{melody; chords} = to_lilypond structure in
        (melody ^ " \\fine", chords, None)
  in
  lwt (
    spf
      "<< \\new Voice {\\clef treble \\time %s \\key %s {%s}}\\new ChordNames {\\chordmode {%s}}>>"
      time
      key
      melody
      chords,
    instructions
  )

let version_to_lilypond_content ~version_params version =
  let content = Model.Version.content version in
  (* get a LilyPond from the potentially-destructured content *)
  let%lwt (content, instructions) =
    match content with
    | Monolithic {lilypond; _} -> lwt (lilypond, None)
    | Destructured {parts; transitions; _} -> version_parts_to_lilypond_content ~version_params version parts transitions
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
  let%lwt slug = Entry.Slug.to_string <$> Model.Version.slug version in
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
  let stylesheet = "/fonts.css" in
  let%lwt tune = Model.Version.tune version in
  let kind = Model.Tune.kind' tune in
  let (tempo_unit, tempo_value) = Kind.Base.tempo kind in
  let chords_kind = Kind.Base.to_pretty_string ~capitalised: false kind in
  let show_bar_numbers =
    Model.Version.(Content.is_monolithic @@ content version)
    || Model.VersionParameters.structure version_params <> None
  in
  lwt Renderer.{slug; name; instructions; composer; content; first_bar; stylesheet; tempo_unit; tempo_value; chords_kind; show_bar_numbers}

let version_to_renderer_tune' ?version_params version =
  version_to_renderer_tune ?version_params (Entry.value version)

let part_to_renderer_part name =
  Renderer.{name = NEString.to_string name}

let set_to_renderer_set set set_params =
  let slug = Entry.Slug.to_string @@ Model.Set.slug set in
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
  let slug = Entry.Slug.(to_string % of_string) name in
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
  let slug = Entry.Slug.to_string @@ Model.Book.slug book in
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
          Option.to_list (RenderingParameters.instruments rendering_params);
          Option.to_list (RenderingParameters.clef rendering_params);
        ]
  in
  let headers = Option.value ~default: true @@ RenderingParameters.show_headers rendering_params in
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
