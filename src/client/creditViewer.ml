open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
open Dancelor_common

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
  let credit = Credit.get slug in

  (* title *)
  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.bind credit Credit.line) page in
    Dom.appendChild content (Text.Heading.root title)
  in

  (* Persons in the credit *)

  let () =
    let pre_text = Text.Paragraph.create ~text:(Lwt.return "contains the following persons:") page in
    Dom.appendChild content (Text.Paragraph.root pre_text);

    let persons_dom = Html.createUl (Page.document page) in
    Dom.appendChild content persons_dom;

    Lwt.on_success (credit >>=| Credit.persons) @@ fun persons ->
    List.iter (fun person ->
        let slug = Person.slug person in
        let href =
          let%lwt slug = slug in
          Lwt.return (Router.path_of_controller (Router.Person slug) |> snd)
        in
        let link = Text.Link.create ~href ~text:(Person.name person) page in
        let li = Html.createLi (Page.document page) in
        Dom.appendChild li (Text.Link.root link);
        Dom.appendChild persons_dom li)
      persons
  in

  (* Tunes composed *)

  let () =
    let pre_text = Text.Paragraph.create ~text:(Lwt.return "composed the following tunes:") page in
    Dom.appendChild content (Text.Paragraph.root pre_text);

    let tunes_dom = Html.createUl (Page.document page) in
    Dom.appendChild content tunes_dom;

    let filter =
      let%lwt credit = credit in
      Tune.Filter.Author (Credit.Filter.Is credit)
      |> Lwt.return
    in
    Lwt.on_success filter @@ fun filter ->
    Lwt.on_success (Tune.all ~filter ()) @@ fun tunes ->

    List.iter (fun tune ->
        let slug = Tune.slug tune in
        let href =
          let%lwt slug = slug in
          Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
        in
        let link = Text.Link.create ~href ~text:(Tune.name tune) page in
        let li = Html.createLi (Page.document page) in
        Dom.appendChild li (Text.Link.root link);
        Dom.appendChild tunes_dom li)
      tunes
  in

  (* Sets devised *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Sets Devised") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let sets_lwt =
      let%lwt credit = credit in
      let filter = Set.Filter.Deviser (Credit.Filter.Is credit) in
      Set.all ~filter ()
    in

    let table = Dancelor_client_tables.Set.make sets_lwt page in

    (* When getting the sets, decide to show just a text or the table *)

    Lwt.on_success sets_lwt @@ fun sets ->
    if sets = [] then
      let text = Text.Paragraph.create ~text:(Lwt.return "There are no sets containing this version.") page in
      Dom.appendChild tableHolder (Text.Paragraph.root text)
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
