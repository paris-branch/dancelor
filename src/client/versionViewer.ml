open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
open Dancelor_common
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
  let version = Version.get slug in
  let tune = Lwt.bind version Version.tune in

  (* title *)
  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.bind tune Tune.name) page in
    let title = Text.Heading.root title in
    title##.classList##add (js "title");
    Dom.appendChild content title
  in

  (* aka *)
  let () =
    let text = Formatters.Tune.aka_lwt tune in
    let aka = Text.Heading.h3_static ~text page in
    let aka = Text.Heading.root aka in
    aka##.classList##add (js "title");
    Dom.appendChild content aka
  in

  (* recommended *)
  let () =
    let text = Formatters.Tune.recommended_lwt tune in
    let recommended = Text.Heading.h3_static ~text page in
    let recommended = Text.Heading.root recommended in
    recommended##.classList##add (js "title");
    Dom.appendChild content recommended
  in

  (* tune description *)
  let () =
    let text =
      let%lwt tune = tune in
      Formatters.Tune.description tune page
    in
    let tune_description = Text.Heading.h3 ~content:text page in
    let tune_description = Text.Heading.root tune_description in
    tune_description##.classList##add (js "title");
    Dom.appendChild content tune_description
  in

  (* Version description *)

  let () =
    let text =
      let%lwt version = version in
      Formatters.Version.description version page
    in
    let version_description = Text.Heading.h3 ~content:text page in
    let version_description = Text.Heading.root version_description in
    version_description##.classList##add (js "title");
    Dom.appendChild content version_description
  in

  (* Buttons *)

  let () =
    let span = Html.createDiv (Page.document page) in
    span##.classList##add (js "buttons");

    let pdf_href, ly_href =
      Helpers.build_path ~api:true ~route:(Router.VersionPdf slug) (),
      Helpers.build_path ~api:true ~route:(Router.VersionLy slug) ()
    in
    let pdf, ly =
      Inputs.Button.create ~href:(Lwt.return pdf_href) ~icon:"file-pdf" ~text:"PDF" page,
      Inputs.Button.create ~href:(Lwt.return ly_href) ~icon:"file-alt" ~text:"LilyPond" page
    in
    Dom.appendChild span (Inputs.Button.root pdf);
    Dom.appendChild span (Inputs.Button.root ly);

    Dom.appendChild content span
  in

  (* Previsualisation *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Previsualisation") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let source =
      spf "/%s%s"
        Constant.api_prefix
        (Router.path_of_controller (Router.VersionSvg slug) |> snd)
      |> Lwt.return
    in
    let img = Image.create ~source page in
    Dom.appendChild content (Image.root img);
  in

  (* Other versions *)

  let other_versions_lwt =
    let%lwt tune = tune in
    let%lwt version = version in
    let filter =
      Formula.(
        and_
          (pred (Version.Filter.Tune (Tune.Filter.Is tune)))
          (not_ (pred (Version.Filter.Is version)))
      )
    in
    Version.all ~filter ()
  in

  let link_page_of_tunes () =
    let href =
      let%lwt tune = tune in
      let%lwt slug = Tune.slug tune in
      Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
    in
    let link = Text.Link.create ~href ~text:(Lwt.return "page of the tune") page in
    Text.Link.root link
  in

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Other Versions") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let table = Dancelor_client_tables.Version.make other_versions_lwt page in

    (* When getting the versions, decide to show just a text or the table *)

    Lwt.on_success other_versions_lwt @@ fun versions ->
    if versions = [] then
      let text = Text.Paragraph.create ~text:(Lwt.return "There are no other versions available for this tune.") page in
      Dom.appendChild tableHolder (Text.Paragraph.root text)
    else
      (
        Dom.appendChild tableHolder (Table.root table);

        let linkHolder = Html.createP (Page.document page) in
        Dom.appendChild linkHolder ((Page.document page)##createTextNode (js "Hint: You can also go to the "));
        Dom.appendChild linkHolder (link_page_of_tunes ());
        Dom.appendChild linkHolder ((Page.document page)##createTextNode (js "."));
        Dom.appendChild tableHolder linkHolder
      )
  in

  (* Sets in which this version can be found *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Sets in Which This Version Appears") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let none = (Page.document page)##createTextNode (js "") in
    let space = (Page.document page)##createTextNode (js " ") in

    let none_and_hint = Html.createP (Page.document page) in
    Dom.appendChild none_and_hint none;
    Dom.appendChild none_and_hint space;
    Dom.appendChild content none_and_hint;

    Lwt.on_success other_versions_lwt
      (fun versions ->
         if versions <> [] then
           (
             Dom.appendChild none_and_hint ((Page.document page)##createTextNode (js "Hint: If you want to see the sets in which this version or any other appear, go to the "));
             Dom.appendChild none_and_hint (link_page_of_tunes ());
             Dom.appendChild none_and_hint ((Page.document page)##createTextNode (js "."))
           ));

    let sets_lwt =
      let%lwt version = version in
      let filter = Set.Filter.ExistsVersion (Version.Filter.Is version) in
      Set.all ~filter ()
    in

    let table = Dancelor_client_tables.Set.make sets_lwt page in

    (* When getting the sets, decide to show just a text or the table *)

    Lwt.on_success sets_lwt @@ fun sets ->
    if sets = [] then
      none##.data := js "There are no sets containing this version."
    else
      Dom.appendChild tableHolder (Table.root table)
  in

  (* Books in which this version can be found *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Books in Which This Version Appears") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    (* Two text nodes; one containing a note that there are no sets containing
       this version, the other explaining to go to the page of the tune if on
       wants that. *)
    let none = (Page.document page)##createTextNode (js "") in
    let space = (Page.document page)##createTextNode (js " ") in
    let hint = (Page.document page)##createTextNode (js "") in

    let none_and_hint = Html.createP (Page.document page) in
    Dom.appendChild none_and_hint none;
    Dom.appendChild none_and_hint space;
    Dom.appendChild none_and_hint hint;
    Dom.appendChild content none_and_hint;

    Lwt.on_success other_versions_lwt
      (fun versions ->
         if versions <> [] then
           (
             Dom.appendChild none_and_hint ((Page.document page)##createTextNode (js "Hint: If you want to see the books in which this version or any other appear, go to the "));
             Dom.appendChild none_and_hint (link_page_of_tunes ());
             Dom.appendChild none_and_hint ((Page.document page)##createTextNode (js "."))
           ));

    let books_lwt =
      let%lwt version = version in
      let filter = Book.Filter.ExistsVersion (Version.Filter.Is version) in
      Book.all ~filter ()
    in

    let table = Dancelor_client_tables.Book.make books_lwt page in

    (* When getting the books, decide to show just a text or the table *)

    Lwt.on_success books_lwt @@ fun books ->
    if books = [] then
      none##.data := js "There are no books containing this version."
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
