open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

let js = Js.string

type t =
  {
    page : Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in
  let tune_lwt = Tune.get slug in

  Lwt.async (fun () ->
      let%lwt tune = tune_lwt in
      let%lwt name = Tune.name tune in
      document##.title := js (name ^ " | Tune | Dancelor");
      Lwt.return ()
    );

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [L.txt (tune_lwt >>=| Tune.name)];
      L.h3 ~a:[a_class ["title"]] (tune_lwt >>=| Formatters.TuneNewAPI.aka);
      L.h3 ~a:[a_class ["title"]] (tune_lwt >>=| Formatters.TuneNewAPI.description);
      L.div (
        match%lwt tune_lwt >>=| Tune.scddb_id with
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
          let versions_lwt =
            let%lwt filter =
              let%lwt tune = tune_lwt in
              Lwt.return (Version.Filter.tuneIs tune)
            in
            Version.search filter
            >|=| Score.list_erase
          in
          let%lwt versions = versions_lwt in

          (* If there is only one version, redirect directly to it. *)
          if List.length versions = 1 then
            (
              Lwt.async @@ fun () ->
              let%lwt href =
                let%lwt slug = Version.slug (List.hd versions) in
                Lwt.return PageRouter.(path (Version slug))
              in
              Dom_html.window##.location##.href := js href;
              Lwt.return_unit
            );

          Lwt.return [Dancelor_client_tables.versions versions]
        )
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Dances That Recommend This Tune"];

        L.div (
          let none = (Page.document page)##createTextNode (js "") in
          let none_maybe = Dom_html.createP (Page.document page) in
          Dom.appendChild none_maybe none;
          Dom.appendChild content none_maybe;

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
          let none = (Page.document page)##createTextNode (js "") in
          let none_maybe = Dom_html.createP (Page.document page) in
          Dom.appendChild none_maybe none;
          Dom.appendChild content none_maybe;

          let sets_lwt =
            let%lwt tune = tune_lwt in
            let filter = Set.Filter.existsVersion (Version.Filter.tuneIs tune) in
            Set.search filter
            >|=| Score.list_erase
          in
          let%lwt sets = sets_lwt in

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
            let filter = Book.Filter.memTuneDeep tune in
            Book.search filter
            >|=| Score.list_erase
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
