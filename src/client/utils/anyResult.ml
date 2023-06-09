open Lwt
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
module Router = Dancelor_client_router
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

let make_credit_result ~prefix page credit =
  let%lwt slug = Credit.slug credit in
  let href = Lwt.return Router.(path (Credit slug)) in
  let cells =
    prefix @ [
      Table.Cell.text ~colspan:3 ~text:(Credit.line credit) page
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_dance_result ~prefix page dance =
  let%lwt slug = Dance.slug dance in
  let href = Lwt.return Router.(path (Dance slug)) in
  let cells =
    prefix @ [
      Table.Cell.text ~text:(Dance.name dance) page ;
      Table.Cell.text ~text:(Dance.kind dance >|= Kind.dance_to_string) page ;
      Table.Cell.create ~content:(
        let%lwt deviser = Dance.deviser dance in
        let%lwt content = Formatters.Credit.line deviser in
        Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
      ) page ;
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_person_result ~prefix page person =
  let%lwt slug = Person.slug person in
  let href = Lwt.return Router.(path (Person slug)) in
  let cells =
    prefix @ [
      Table.Cell.text ~colspan:3 ~text:(Person.name person) page
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_book_result ~prefix page book =
  let%lwt slug = Book.slug book in
  let href = Lwt.return Router.(path (Book slug)) in
  let cells =
    prefix @ [
      Table.Cell.create ~colspan:3 ~content:(
        let%lwt content = Formatters.Book.title_and_subtitle book in
        Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
      ) page
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_set_result ~prefix page set =
  let%lwt slug = Set.slug set in
  let href = Lwt.return Router.(path (Set slug)) in
  let cells =
    prefix @ [
      Table.Cell.text ~text:(Set.name set) page;
      Table.Cell.text ~text:(Set.kind set >|= Kind.dance_to_string) page ;
      Table.Cell.create ~content:(
        let%lwt deviser = Set.deviser set in
        let%lwt content = Formatters.Credit.line deviser in
        Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
      ) page;
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_tune_result ~prefix page tune =
  let%lwt slug = Tune.slug tune in
  let href = Lwt.return Router.(path (Tune slug)) in
  let cells =
    prefix @ [
      Table.Cell.text ~text:(Tune.name tune) page ;
      Table.Cell.text ~text:(Tune.kind tune >|= Kind.base_to_pretty_string ~capitalised:true) page ;
      Table.Cell.create ~content:(
        let%lwt author = Tune.author tune in
        let%lwt content = Formatters.Credit.line author in
        Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
      ) page ;
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_version_result ~prefix page version =
  let%lwt slug = Version.slug version in
  let href = Lwt.return Router.(path (Version slug)) in
  let%lwt tune = Version.tune version in
  let cells =
    prefix @ [
      Table.Cell.create ~content:(
        let%lwt content = Formatters.Version.name_and_disambiguation ~link:false version in
        Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
      ) page;
      Table.Cell.text ~text:(
        let%lwt bars = Version.bars version in
        let%lwt kind = Tune.kind tune in
        let%lwt structure = Version.structure version in
        Lwt.return (Kind.version_to_string (bars, kind) ^ " (" ^ structure ^ ")")
      ) page ;
      Table.Cell.create ~content:(
        let%lwt content = Formatters.Version.author_and_arranger version in
        Lwt.return (Dancelor_client_html.nodes_to_dom_nodes (Page.document page) content)
      ) page;
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_result page score =
  let any = Score.value score in
  let prefix = [
    Table.Cell.text ~text:(Lwt.return (Score.score_to_string score)) page ;
    Table.Cell.text ~text:(Lwt.return (any |> Any.type_of |> Any.Type.to_string)) page ;
  ]
  in
  match any with
  | Credit credit   -> make_credit_result  ~prefix page credit
  | Dance dance     -> make_dance_result   ~prefix page dance
  | Person person   -> make_person_result  ~prefix page person
  | Book book       -> make_book_result    ~prefix page book
  | Set set         -> make_set_result     ~prefix page set
  | Tune tune       -> make_tune_result    ~prefix page tune
  | Version version -> make_version_result ~prefix page version
