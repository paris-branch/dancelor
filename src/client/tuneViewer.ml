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

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] [ text_lwt (tune_lwt >>=| Tune.name) ];
      h3_lwt ~classes:["title"] (tune_lwt >>=| Formatters.Tune.aka);
      h3_lwt ~classes:["title"] (tune_lwt >>=| Formatters.Tune.recommended);
      h3_lwt ~classes:["title"] (tune_lwt >>=| Formatters.Tune.description);

      div ~classes:["section"] [
        h3 [ text "Versions of This Tune" ];

        div_lwt (
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
                Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
              in
              Dom_html.window##.location##.href := js href;
              Lwt.return_unit
            );

          Lwt.return [ Dancelor_client_tables.versions versions ]
        )
      ];

      div ~classes:["section"] [
        h3 [ text "Sets in Which This Tune Appears" ];

        div_lwt (
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
              text "There are no sets containing this tune."
            else
              Dancelor_client_tables.sets sets
          ]
        )
      ];

      div ~classes:["section"] [
        h3 [ text "Books in Which This Tune Appears" ];

        div_lwt (
          let%lwt books =
            let%lwt tune = tune_lwt in
            let filter = Book.Filter.memTuneDeep tune in
            Book.search filter
            >|=| Score.list_erase
          in

          Lwt.return [
            if books = [] then
              text "There are no books containing this tune."
            else
              Dancelor_client_tables.books books
          ]
        )
      ]
    ]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
