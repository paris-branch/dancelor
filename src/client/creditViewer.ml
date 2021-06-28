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

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
