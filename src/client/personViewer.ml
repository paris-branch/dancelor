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
  let person = Person.get slug in

  (* title *)
  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.bind person Person.name) page in
    Dom.appendChild content (Text.Heading.root title)
  in

  (* Credits *)

  let () =
    let pre_text = Text.Paragraph.create ~text:(Lwt.return "appears in the following credits:") page in
    Dom.appendChild content (Text.Paragraph.root pre_text);

    let credits_dom = Html.createUl (Page.document page) in
    Dom.appendChild content credits_dom;

    (* Create the Lwt value containing the request to the credits *)
    let filter =
      let%lwt person = person in
      Credit.Filter.ExistsPerson (Person.Filter.Is person)
      |> Lwt.return
    in
    Lwt.on_success filter @@ fun filter ->
    Lwt.on_success (Credit.all ~filter ()) @@ fun credits ->

    List.iter (fun credit ->
        let slug = Credit.slug credit in
        let href =
          let%lwt slug = slug in
          Lwt.return (Router.path_of_controller (Router.Credit slug) |> snd)
        in
        let link = Text.Link.create ~href ~text:(Credit.line credit) page in
        let li = Html.createLi (Page.document page) in
        Dom.appendChild li (Text.Link.root link);
        Dom.appendChild credits_dom li)
      credits
  in

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
