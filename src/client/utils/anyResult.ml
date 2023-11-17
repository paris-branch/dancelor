open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

let make_person_result ~prefix page person =
  let slug = Person.slug person in
  let href = Lwt.return PageRouter.(path (Person slug)) in
  let cells =
    prefix @ [
      Table.Cell.text ~colspan:3 ~text:(Lwt.return @@ Person.name person) page
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_dance_result ~prefix page dance =
  let href = Lwt.return @@ PageRouter.path @@ PageRouter.Dance (Dance.slug dance) in
  let cells =
    prefix @ [
      Table.Cell.text ~text:(Lwt.return @@ Dance.name dance) page ;
      Table.Cell.text ~text:(Lwt.return @@ Kind.Dance.to_string @@ Dance.kind dance) page ;
      Table.Cell.create ~content:(
        Dancelor_client_html.to_old_style
          (Lwt.map Formatters.Person.name (Dance.deviser dance))
      ) page ;
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_book_result ~prefix page book =
  let href = Lwt.return @@ PageRouter.path @@ PageRouter.Book (Book.slug book) in
  let cells =
    prefix @ [
      Table.Cell.create ~colspan:3 ~content:(
        Dancelor_client_html.to_old_style
          (Lwt.return @@ Formatters.Book.title_and_subtitle book)
      ) page
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_set_result ~prefix page set =
  let href = Lwt.return @@ PageRouter.path @@ PageRouter.Set (Set.slug set) in
  let cells =
    prefix @ [
      Table.Cell.text ~text:(Lwt.return @@ Set.name set) page;
      Table.Cell.text ~text:(Lwt.return @@ Kind.Dance.to_string @@ Set.kind set) page ;
      Table.Cell.create ~content:(
        Dancelor_client_html.to_old_style
          (Lwt.map Formatters.Person.name (Set.deviser set))
      ) page;
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_tune_result ~prefix page tune =
  let href = Lwt.return @@ PageRouter.path @@ PageRouter.Tune (Tune.slug tune) in
  let cells =
    prefix @ [
      Table.Cell.text ~text:(Lwt.return @@ Tune.name tune) page ;
      Table.Cell.text ~text:(Lwt.return @@ Kind.Base.to_pretty_string ~capitalised:true @@ Tune.kind tune) page ;
      Table.Cell.create ~content:(
        Dancelor_client_html.to_old_style
          (Lwt.map Formatters.Person.name (Tune.author tune))
      ) page ;
    ]
  in
  Lwt.return (Table.Row.create ~href ~cells page)

let make_version_result ~prefix page version =
  let href = Lwt.return @@ PageRouter.path @@ PageRouter.Version (Version.slug version) in
  let%lwt tune = Version.tune version in
  let cells =
    prefix @ [
      Table.Cell.create ~content:(
        Dancelor_client_html.to_old_style
          (Formatters.Version.name_and_disambiguation ~link:false version)
      ) page;
      Table.Cell.text ~text:(
        let bars = Version.bars version in
        let structure = Version.structure version in
        Lwt.return (Kind.Version.to_string (bars, Tune.kind tune) ^ " (" ^ structure ^ ")")
      ) page ;
      Table.Cell.create ~content:(
        Dancelor_client_html.to_old_style
          (Formatters.Version.author_and_arranger version)
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
  | Person person   -> make_person_result  ~prefix page person
  | Dance dance     -> make_dance_result   ~prefix page dance
  | Book book       -> make_book_result    ~prefix page book
  | Set set         -> make_set_result     ~prefix page set
  | Tune tune       -> make_tune_result    ~prefix page tune
  | Version version -> make_version_result ~prefix page version
