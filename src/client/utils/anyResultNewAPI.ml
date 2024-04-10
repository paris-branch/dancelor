open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
open Dancelor_client_html

module Html = Dom_html

let js = Js.string

(* FIXME: this is very similar to [Dancelor_client_tables.clickable_row]; those
   two should be merged in a common notion (probably that ot
   [Dancelor_client_tables]). *)
(* FIXME: When [onclick] is used as an [a], we could do better and actually have
   an [<a />] element *)
let clickable_row ?(onclick = fun () -> ()) =
  tr
    ~a:[
      a_class ["clickable"];
      a_onclick (fun _ -> onclick (); true);
    ]

let make_person_result' ?onclick ?(prefix=[]) ?(suffix=[]) person =
  clickable_row ?onclick
    (
      prefix @ [
        td ~a:[a_colspan 3] (Formatters.Person.name ~link:false person);
      ] @ suffix
    )

let make_person_result ?context ?prefix ?suffix person =
  make_person_result'
    ~onclick: (fun () ->
        let context = Option.map S.value context in
        let href = PageRouter.path_person ?context @@ Person.slug person in
        Dom_html.window##.location##.href := Js.string href
      )
    ?prefix
    ?suffix
    person

let make_dance_result' ?onclick ?(prefix=[]) ?(suffix=[]) dance =
  clickable_row ?onclick
    (
      prefix @ [
        td [txt (Dance.name dance)];
        td [txt (Kind.Dance.to_string @@ Dance.kind dance)];
        L.td (Lwt.map (Formatters.Person.names ~short:true) (Dance.devisers dance));
      ] @ suffix
    )

let make_dance_result ?context ?prefix ?suffix dance =
  make_dance_result'
    ~onclick: (fun () ->
        let context = Option.map S.value context in
        let href = PageRouter.path_dance ?context @@ Dance.slug dance in
        Dom_html.window##.location##.href := Js.string href
      )
    ?prefix
    ?suffix
    dance

let make_book_result' ?onclick ?(prefix=[]) ?(suffix=[]) book =
  clickable_row ?onclick
    (
      prefix @ [
        td (Formatters.Book.title_and_subtitle book);
        td ~a:[a_colspan 2] [txt (Option.fold ~none:"" ~some:PartialDate.to_pretty_string (Book.date book))];
      ] @ suffix
    )

let make_book_result ?context ?prefix ?suffix book =
  make_book_result'
    ~onclick: (fun () ->
        let context = Option.map S.value context in
        let href = PageRouter.path_book ?context @@ Book.slug book in
        Dom_html.window##.location##.href := Js.string href
      )
    ?prefix
    ?suffix
    book

let make_set_result' ?onclick ?(prefix=[]) ?(suffix=[]) set =
  clickable_row ?onclick
    (
      prefix @ [
        td [txt @@ Set.name set];
        td [txt @@ Kind.Dance.to_string @@ Set.kind set];
        L.td (Lwt.map (Formatters.Person.names ~short:true) (Set.conceptors set));
      ] @ suffix
    )

let make_set_result ?context ?prefix ?suffix set =
  make_set_result'
    ~onclick: (fun () ->
        let context = Option.map S.value context in
        let href = PageRouter.path_set ?context @@ Set.slug set in
        Dom_html.window##.location##.href := Js.string href
      )
    ?prefix
    ?suffix
    set

let make_tune_result' ?onclick ?(prefix=[]) ?(suffix=[]) tune =
  clickable_row ?onclick
    (
      prefix @ [
        td [txt @@ Tune.name tune];
        td [txt @@ Kind.Base.to_pretty_string ~capitalised:true @@ Tune.kind tune];
        L.td (Formatters.Tune.composers tune);
      ] @ suffix
    )

let make_tune_result ?context ?prefix ?suffix tune =
  make_tune_result'
    ~onclick: (fun () ->
        let context = Option.map S.value context in
        let href = PageRouter.path_tune ?context @@ Tune.slug tune in
        Dom_html.window##.location##.href := Js.string href
      )
    ?prefix
    ?suffix
    tune

let make_version_result' ?onclick ?(prefix=[]) ?(suffix=[]) version =
  clickable_row ?onclick
    (
      prefix @ [
        L.td (Formatters.Version.name_and_disambiguation ~link:false version);
        td [
          L.txt (
            let bars = Version.bars version in
            let%lwt kind = Lwt.map Tune.kind @@ Version.tune version in
            let structure = Version.structure version in
            Lwt.return (Kind.Version.to_string (bars, kind) ^ " (" ^ structure ^ ")")
          )
        ];
        L.td (Formatters.Version.composer_and_arranger ~short:true version);
      ] @ suffix
    )

let make_version_result ?context ?prefix ?suffix version =
  make_version_result'
    ~onclick: (fun () ->
        let context = Option.map S.value context in
        let href = PageRouter.path_version ?context @@ Version.slug version in
        Dom_html.window##.location##.href := Js.string href
      )
    ?prefix
    ?suffix
    version

let any_type_to_fa = function
  | Any.Type.Person -> "person"
  | Dance -> "directions_walk"
  | Tune -> "music_note"
  | Version -> "music_note"
  | Set -> "format_list_bulleted"
  | Book -> "library_books"

let make_result ?context any =
  let type_ = Any.type_of any in
  let prefix = [
    td [
      i ~a:[a_class ["material-symbols-outlined"]] [txt @@ any_type_to_fa type_];
      txt " ";
      txt (Any.Type.to_string type_);
    ];
  ]
  in
  match any with
  | Person person   -> make_person_result  ?context ~prefix person
  | Dance dance     -> make_dance_result   ?context ~prefix dance
  | Book book       -> make_book_result    ?context ~prefix book
  | Set set         -> make_set_result     ?context ~prefix set
  | Tune tune       -> make_tune_result    ?context ~prefix tune
  | Version version -> make_version_result ?context ~prefix version
