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
    let credits_text = Html.createP (Page.document page) in
    Dom.appendChild content credits_text;

    Dom.appendChild credits_text ((Page.document page)##createTextNode (js "This is the page of a person involved in "));

    (* Create the Lwt value containing the request to the credits *)
    let filter =
      let%lwt person = person in
      Credit.Filter.ExistsPerson (Person.Filter.Is person)
      |> Lwt.return
    in
    Lwt.on_success filter @@ fun filter ->
    Lwt.on_success (Credit.all ~filter ()) @@ fun credits ->

    let link_of_credit credit =
      let slug = Credit.slug credit in
      let href =
        let%lwt slug = slug in
        Lwt.return (Router.path_of_controller (Router.Credit slug) |> snd)
      in
      let link = Text.Link.create ~href ~text:(Credit.line credit) page in
      Text.Link.root link
    in

    match credits with
    | [] ->
      Dom.appendChild credits_text ((Page.document page)##createTextNode (js "no credits. Why this person exists in the database is a mystery!"))

    | [credit] ->
      Dom.appendChild credits_text ((Page.document page)##createTextNode (js "the sole credit "));
      Dom.appendChild credits_text (link_of_credit credit);
      Dom.appendChild credits_text ((Page.document page)##createTextNode (js "."))

    | first_credit :: credits ->
      let last_credit = List.ft credits in
      let credits = List.bd credits in

      Dom.appendChild credits_text ((Page.document page)##createTextNode (js "the credits "));
      Dom.appendChild credits_text (link_of_credit first_credit);

      List.iter
        (fun credit ->
           Dom.appendChild credits_text ((Page.document page)##createTextNode (js ", "));
           Dom.appendChild credits_text (link_of_credit credit))
        credits;

      Dom.appendChild credits_text ((Page.document page)##createTextNode (js " and "));
      Dom.appendChild credits_text (link_of_credit last_credit);
      Dom.appendChild credits_text ((Page.document page)##createTextNode (js ". The current page will contain all the work this person is involved in. Visit the page of the credits for work done specifically as that particular credit."))
  in

  (* Tunes Composed *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Tunes Composed") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let tunes_lwt =
      let%lwt person = person in
      let filter = Tune.Filter.Author (Credit.Filter.ExistsPerson (Person.Filter.Is person)) in
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
      let%lwt person = person in
      let filter = Set.Filter.Deviser (Credit.Filter.ExistsPerson (Person.Filter.Is person)) in
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
