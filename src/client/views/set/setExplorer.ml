open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_components
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Html.divElement Js.t;
  }

let create page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Html.createDiv document in

  document##.title := js "All sets | Dancelor";

  let pagination =
    PageNav.create
      ~number_of_entries: (Dancelor_client_html.S.from' 0 @@ Set.count Formula.true_)
      ~entries_per_page: 25
  in

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [txt "All sets"];

      PageNav.render pagination;

      tablex
        ~a:[a_class ["separated-table"]]
        ~thead:(
          thead [
            tr [
              th [
                span ~a:[a_class ["full-content"]] [txt "Name"];
                span ~a:[a_class ["collapse-content"]] [txt "Sets"];
              ];
              th [txt "Deviser"];
              th [txt "Kind"];
              th [txt "Actions"];
            ]
          ]
        )
        [
          R.tbody (
            S.bind_s' (PageNav.pagination pagination) [] @@ fun pagination ->
            Fun.flip Lwt.map
              (Set.search' ~pagination Formula.true_ >|=| Score.list_erase)
              (List.map
                 (fun set ->
                    let href = PageRouter.path @@ PageRouter.Set (Set.slug set) in
                    Dancelor_client_tables.clickable_row ~href [
                      (Formatters.Set.name_and_tunes ~link:false set);
                      (Lwt.map Formatters.Person.name (Set.deviser set));
                      Lwt.return [txt @@ Kind.Dance.to_string @@ Set.kind set];
                      Lwt.return [txt ""];
                    ]
                 )
              )
          )
        ];

      PageNav.render pagination;
    ]
  );

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
