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
let clickable_row ~href =
  tr
    ~a:[
      a_class ["clickable"];
      a_onclick
        (fun _ ->
           let open Js_of_ocaml in
           Dom_html.window##.location##.href := Js.string (S.value href);
           true
        );
    ]

let s_for_option so f = match so with
  | None -> S.const (f None)
  | Some s -> S.map (f % Option.some) s

let make_person_result ?context ~prefix person =
  clickable_row
    ~href:(
      s_for_option context @@ fun context ->
      PageRouter.path_person ?context @@ Person.slug person
    )
    (
      prefix @ [
        td ~a:[a_colspan 3] (Formatters.Person.name ~link:false (Some person));
      ]
    )

let make_dance_result ?context ~prefix dance =
  clickable_row
    ~href:(
      s_for_option context @@ fun context ->
      PageRouter.path_dance ?context @@ Dance.slug dance
    )
    (
      prefix @ [
        td [txt (Dance.name dance)];
        td [txt (Kind.Dance.to_string @@ Dance.kind dance)];
        L.td (Lwt.map (Formatters.Person.names ~short:true) (Dance.devisers dance));
      ]
    )

let make_book_result ?context ~prefix book =
  clickable_row
    ~href:(
      s_for_option context @@ fun context ->
      PageRouter.path_book ?context @@ Book.slug book
    )
    (
      prefix @ [
        td ~a:[a_colspan 3] (Formatters.Book.title_and_subtitle book);
      ]
    )

let make_set_result ?context ~prefix set =
  clickable_row
    ~href:(
      s_for_option context @@ fun context ->
      PageRouter.path_set ?context @@ Set.slug set
    )
    (
      prefix @ [
        td [txt @@ Set.name set];
        td [txt @@ Kind.Dance.to_string @@ Set.kind set];
        L.td (Lwt.map (Formatters.Person.names ~short:true) (Set.devisers set));
      ]
    )

let make_tune_result ?context ~prefix tune =
  clickable_row
    ~href:(
      s_for_option context @@ fun context ->
      PageRouter.path_tune ?context @@ Tune.slug tune
    )
    (
      prefix @ [
        td [txt @@ Tune.name tune];
        td [txt @@ Kind.Base.to_pretty_string ~capitalised:true @@ Tune.kind tune];
        L.td (Formatters.Tune.composers tune);
      ]
    )

let make_version_result ?context ~prefix version =
  clickable_row
    ~href:(
      s_for_option context @@ fun context ->
      PageRouter.path_version ?context @@ Version.slug version
    )
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
        L.td (Formatters.Version.composer_and_arranger version);
      ]
    )

let make_result ?context any =
  let prefix = [
    td [txt (any |> Any.type_of |> Any.Type.to_string)];
  ]
  in
  match any with
  | Person person   -> make_person_result  ?context ~prefix person
  | Dance dance     -> make_dance_result   ?context ~prefix dance
  | Book book       -> make_book_result    ?context ~prefix book
  | Set set         -> make_set_result     ?context ~prefix set
  | Tune tune       -> make_tune_result    ?context ~prefix tune
  | Version version -> make_version_result ?context ~prefix version
