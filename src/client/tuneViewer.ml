open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

module Html = Dom_html

let js = Js.string

type t =
{
  page : Page.t;
  content : Html.divElement Js.t;
}

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let tune = Tune.get slug in

  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.bind tune Tune.name) page in
    let title = Text.Heading.root title in
    title##.classList##add (js "title");
    Dom.appendChild content title
  in

  (* Aka *)

  let () =
    let text = Formatters.Tune.aka_lwt tune in
    let aka = Text.Heading.h3_static ~text page in
    let aka = Text.Heading.root aka in
    aka##.classList##add (js "title");
    Dom.appendChild content aka
  in

  (* Recommended *)

  let () =
    let text = Formatters.Tune.recommended_lwt tune in
    let recommended = Text.Heading.h3_static ~text page in
    let recommended = Text.Heading.root recommended in
    recommended##.classList##add (js "title");
    Dom.appendChild content recommended
  in

  (* Description *)

  let () =
    let text =
      let%lwt tune = tune in
      Formatters.Tune.description tune page
    in
    let description = Text.Heading.h3 ~content:text page in
    let description = Text.Heading.root description in
    description##.classList##add (js "title");
    Dom.appendChild content description
  in

  let versions_lwt =
    let%lwt filter =
      let%lwt tune = tune in
      Version.Filter.Tune (Tune.Filter.Is tune)
      |> Formula.pred
      |> Lwt.return
    in
    Version.all ~filter ()
  in

  let table = Dancelor_client_tables.Version.make versions_lwt page in
  Dom.appendChild content (Table.root table);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
