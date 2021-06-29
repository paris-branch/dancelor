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
    let persons_text = Html.createP (Page.document page) in
    Dom.appendChild content persons_text;

    Dom.appendChild persons_text ((Page.document page)##createTextNode (js "This is the page of a credit containing "));

    Lwt.on_success (credit >>=| Credit.persons) @@ fun persons ->

    let link_of_person person =
      let slug = Person.slug person in
      let href =
        let%lwt slug = slug in
        Lwt.return (Router.path_of_controller (Router.Person slug) |> snd)
      in
      let link = Text.Link.create ~href ~text:(Person.name person) page in
      Text.Link.root link
    in

    match persons with

    | [] ->
      Dom.appendChild persons_text ((Page.document page)##createTextNode (js "no persons."))

    | [person] ->
      let link () = link_of_person person in
      Dom.appendChild persons_text ((Page.document page)##createTextNode (js "only the person "));
      Dom.appendChild persons_text (link ());
      Dom.appendChild persons_text ((Page.document page)##createTextNode (js ". This page and the specific page of "));
      Dom.appendChild persons_text (link ());
      Dom.appendChild persons_text ((Page.document page)##createTextNode (js " are different: the latter will contain all the work "));
      Dom.appendChild persons_text (link ());
      Dom.appendChild persons_text ((Page.document page)##createTextNode (js " is involved in, while the former will contain only the work "));
      Dom.appendChild persons_text (link ());
      Dom.appendChild persons_text ((Page.document page)##createTextNode (js " is involved in as this particular credit."))

    | first_person :: persons ->
      let last_person = List.ft persons in
      let persons = List.bd persons in

      Dom.appendChild persons_text (link_of_person first_person);
      List.iter
        (fun person ->
           Dom.appendChild persons_text ((Page.document page)##createTextNode (js ", "));
           Dom.appendChild persons_text (link_of_person person))
        persons;

      Dom.appendChild persons_text ((Page.document page)##createTextNode (js " and "));
      Dom.appendChild persons_text (link_of_person last_person);
      Dom.appendChild persons_text ((Page.document page)##createTextNode (js ". Visit the specific pages of the individual persons to see their personal work."))
  in

  (* Tunes Composed *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Tunes Composed") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let tunes_lwt =
      let%lwt credit = credit in
      let filter = Formula.(pred (Tune.Filter.Author (pred (Credit.Filter.Is credit)))) in
      Tune.all ~filter ()
    in

    let table = Dancelor_client_tables.Tune.make tunes_lwt page in

    (* When getting the sets, decide to show just a text or the table *)

    Lwt.on_success tunes_lwt @@ fun tunes ->
    if tunes = [] then
      let text = Text.Paragraph.create ~text:(Lwt.return "There are no tunes composed by this credit.") page in
      Dom.appendChild tableHolder (Text.Paragraph.root text)
    else
      Dom.appendChild tableHolder (Table.root table)
  in

  (* Sets devised *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Sets Devised") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let sets_lwt =
      let%lwt credit = credit in
      let filter = Set.Filter.deviser (Credit.Filter.is credit) in
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
