open Js_of_ocaml
open Dancelor_common
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

  let pretext = Text.Heading.h3_static ~text:(Lwt.return "Versions of this Tune") page in
  Dom.appendChild content (Text.Heading.root pretext);

  let versions_lwt =
    let%lwt filter =
      let%lwt tune = tune in
      Lwt.return (Version.Filter.tuneIs tune)
    in
    Version.all ~filter ()
  in

  (* If only one version, redirect to it *)
  Lwt.on_success versions_lwt
    (fun versions ->
       if List.length versions = 1 then
         (
           Lwt.async @@ fun () ->
           let%lwt href =
             let%lwt slug = Version.slug (List.hd versions) in
             Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
           in
           Html.window##.location##.href := js href;
           Lwt.return_unit
         ));

  let table = Dancelor_client_tables.Version.make versions_lwt page in
  Dom.appendChild content (Table.root table);

  (* Sets in which this tune can be found *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Sets in Which This Tune Appears") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let none = (Page.document page)##createTextNode (js "") in
    let none_maybe = Html.createP (Page.document page) in
    Dom.appendChild none_maybe none;
    Dom.appendChild content none_maybe;

    let sets_lwt =
      let%lwt tune = tune in
      let filter = Set.Filter.existsVersion (Version.Filter.tuneIs tune) in
      Set.all ~filter ()
    in

    let table = Dancelor_client_tables.Set.make sets_lwt page in

    (* When getting the sets, decide to show just a text or the table *)

    Lwt.on_success sets_lwt @@ fun sets ->
    if sets = [] then
      none##.data := js "There are no sets containing this tune."
    else
      Dom.appendChild tableHolder (Table.root table)
  in

  (* Books in which this version can be found *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Books in Which This Tune Appears") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let none = (Page.document page)##createTextNode (js "") in
    let none_maybe = Html.createP (Page.document page) in
    Dom.appendChild none_maybe none;
    Dom.appendChild content none_maybe;

    let books_lwt =
      let%lwt tune = tune in
      let filter = Book.Filter.memTuneDeep tune in
      Book.all ~filter ()
    in

    let table = Dancelor_client_tables.Book.make books_lwt page in

    (* When getting the books, decide to show just a text or the table *)

    Lwt.on_success books_lwt @@ fun books ->
    if books = [] then
      none##.data := js "There are no books containing this tune."
    else
      Dom.appendChild tableHolder (Table.root table)
  in

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
