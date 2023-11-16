open Lwt.Infix
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
open Dancelor_client_html

module Html = Dom_html

let js = Js.string

let clickable_row ~href =
  tr
    ~a:[
      a_class ["clickable"];
      a_onclick
        (fun _ ->
           let open Js_of_ocaml in
           Lwt.on_success href (fun href ->
               Dom_html.window##.location##.href := Js.string href);
           true
        );
    ]

let make_person_result ~prefix person =
  clickable_row
    ~href:(Lwt.return @@ PageRouter.path @@ PageRouter.Person (Person.slug person))
    (
      prefix @ [
        L.td ~a:[a_colspan 3] (Formatters.Person.name (Some person));
      ]
    )

let make_dance_result ~prefix dance =
  clickable_row
    ~href:(Dance.slug dance >|= fun slug -> PageRouter.(path (Dance slug)))
    (
      prefix @ [
        td [L.txt (Dance.name dance)];
        td [L.txt (Kind.Dance.to_string =|< Dance.kind dance)];
        L.td (Formatters.Person.name =<< Dance.deviser dance);
      ]
    )

let make_book_result ~prefix book =
  clickable_row
    ~href:(Book.slug book >|= fun slug -> PageRouter.(path (Book slug)))
    (
      prefix @ [
        L.td ~a:[a_colspan 3] (Formatters.Book.title_and_subtitle book);
      ]
    )

let make_set_result ~prefix set =
  clickable_row
    ~href:(Set.slug set >|= fun slug -> PageRouter.(path (Set slug)))
    (
      prefix @ [
        td [L.txt (Set.name set)];
        td [L.txt (Kind.Dance.to_string =|< Set.kind set)];
        L.td (Formatters.Person.name =<< Set.deviser set);
      ]
    )

let make_tune_result ~prefix tune =
  clickable_row
    ~href:(Tune.slug tune >|= fun slug -> PageRouter.(path (Tune slug)))
    (
      prefix @ [
        td [L.txt (Tune.name tune)];
        td [L.txt (Kind.Base.to_pretty_string ~capitalised:true =|< Tune.kind tune)];
        L.td (Formatters.Person.name =<< Tune.author tune);
      ]
    )

let make_version_result ~prefix version =
  clickable_row
    ~href:(Version.slug version >|= fun slug -> PageRouter.(path (Version slug)))
    (
      prefix @ [
        L.td (Formatters.Version.name_and_disambiguation ~link:false version);
        td [
          L.txt (
            let%lwt bars = Version.bars version in
            let%lwt kind = Tune.kind =<< Version.tune version in
            let%lwt structure = Version.structure version in
            Lwt.return (Kind.Version.to_string (bars, kind) ^ " (" ^ structure ^ ")")
          )
        ];
        L.td (Formatters.Version.author_and_arranger version);
      ]
    )

let make_result score =
  let any = Score.value score in
  let prefix = [
    td [txt (Score.score_to_string score)];
    td [txt (any |> Any.type_of |> Any.Type.to_string)];
  ]
  in
  match any with
  | Person person   -> make_person_result  ~prefix person
  | Dance dance     -> make_dance_result   ~prefix dance
  | Book book       -> make_book_result    ~prefix book
  | Set set         -> make_set_result     ~prefix set
  | Tune tune       -> make_tune_result    ~prefix tune
  | Version version -> make_version_result ~prefix version
