open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Components = Dancelor_client_components

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let create ?context slug page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  let tune_lwt = Tune.get slug in

  Lwt.on_success tune_lwt (fun tune ->
      document##.title := js (Tune.name tune ^ " | Tune | Dancelor");
    );

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (PageRouter.path_tune slug)
        (Lwt.map Any.tune tune_lwt);

      h2 ~a:[a_class ["title"]] [L.txt @@ Lwt.map Tune.name tune_lwt];
      L.h3 ~a:[a_class ["title"]] (Lwt.map Formatters.Tune.aka tune_lwt);
      L.h3 ~a:[a_class ["title"]] (tune_lwt >>=| Formatters.Tune.description);
      L.div (
        match%lwt Lwt.map Tune.scddb_id tune_lwt with
        | None -> Lwt.return_nil
        | Some scddb_id ->
          let href = SCDDB.tune_uri scddb_id in
          Lwt.return [
            h3 ~a:[a_class ["title"]] [
              a ~a:[a_href href; a_target "blank"] [
                txt "Link to the Strathspey Database"
              ]
            ]
          ]
      );

      div ~a:[a_class ["section"]] [
        h3 [txt "Versions of This Tune"];

        L.div (
          let%lwt versions =
            let%lwt tune = tune_lwt in
            Version.search' @@ Version.Filter.tuneIs' tune
          in
          Lwt.return [Dancelor_client_tables.versions versions]
        )
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Dances That Recommend This Tune"];

        L.div (
          let%lwt tune = tune_lwt in
          let%lwt dances = Tune.dances tune in

          Lwt.return [
            if dances = [] then
              txt "There are no dances that recommend this tune."
            else
              Dancelor_client_tables.dances dances
          ]
        )
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Sets in Which This Tune Appears"];

        L.div (
          let%lwt sets =
            let%lwt tune = tune_lwt in
            Set.search' @@  Set.Filter.existsVersion' (Version.Filter.tuneIs' tune)
          in

          Lwt.return [
            if sets = [] then
              txt "There are no sets containing this tune."
            else
              Dancelor_client_tables.sets sets
          ]
        )
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Books in Which This Tune Appears"];

        L.div (
          let%lwt books =
            let%lwt tune = tune_lwt in
            Book.search' @@ Book.Filter.memTuneDeep' tune
          in

          Lwt.return [
            if books = [] then
              txt "There are no books containing this tune."
            else
              Dancelor_client_tables.books books
          ]
        )
      ]
    ]
  );

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
