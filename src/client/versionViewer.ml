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
  let version = Version.get slug in
  let tune = Lwt.bind version Version.tune in

  (* title *)
  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.bind tune Tune.name) page in
    Dom.appendChild content (Text.Heading.root title)
  in

  (* aka *)
  let () =
    let text = Formatters.Tune.aka_lwt tune in
    let aka = Text.Heading.h3_static ~text page in
    Dom.appendChild content (Text.Heading.root aka)
  in

  (* recommended *)
  let () =
    let text = Formatters.Tune.recommended_lwt tune in
    let recommended = Text.Heading.h3_static ~text page in
    Dom.appendChild content (Text.Heading.root recommended)
  in

  (* tune description *)
  let () =
    let text =
      let%lwt tune = tune in
      Formatters.Tune.description tune page
    in
    let tune_description = Text.Heading.h3 ~content:text page in
    Dom.appendChild content (Text.Heading.root tune_description)
  in

  (* Version description *)

  let () =
    let text =
      let%lwt version = version in
      Formatters.Version.description version page
    in
    let version_description = Text.Heading.h3 ~content:text page in
    Dom.appendChild content (Text.Heading.root version_description)
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

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Other Versions") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let href =
      let%lwt slug = Lwt.bind tune Tune.slug in
      Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
    in
    let text =
      let%lwt filter =
        let%lwt tune = tune in
        Version.Filter.Tune (Tune.Filter.Is tune)
        |> Formula.pred
        |> Lwt.return
      in
      let%lwt versions = Version.all ~filter () in
      match List.length versions with
      | 1 -> Lwt.return "No other version for this tune"
      | nb ->
        assert (nb > 1);
        Lwt.return (spf "There are %d other versions available for this tune" (nb - 1))
    in
    let title = Text.Link.create ~href ~text page in
    Dom.appendChild content (Text.Link.root title)
  in

  (* Sets in which this version can be found *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Sets in Which This Version Appears") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let sets_dom = Html.createUl (Page.document page) in
    Dom.appendChild content sets_dom;

    let filter =
      let%lwt version = version in
      Set.Filter.ExistsVersion (Version.Filter.Is version)
      |> Lwt.return
    in
    Lwt.on_success filter @@ fun filter ->
    Lwt.on_success (Set.all ~filter ()) @@ fun sets ->

    List.iter (fun set ->
        let slug = Set.slug set in
        let href =
          let%lwt slug = slug in
          Lwt.return (Router.path_of_controller (Router.Set slug) |> snd)
        in
        let link = Text.Link.create ~href ~text:(Set.name set) page in
        let li = Html.createLi (Page.document page) in
        Dom.appendChild li (Text.Link.root link);
        Dom.appendChild sets_dom li)
      sets
  in

  (* Books in which this version can be found *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Books in Which This Version Appears") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let books_dom = Html.createUl (Page.document page) in
    Dom.appendChild content books_dom;

    let filter =
      let%lwt version = version in
      Book.Filter.ExistsVersion (Version.Filter.Is version)
      |> Lwt.return
    in
    Lwt.on_success filter @@ fun filter ->
    Lwt.on_success (Book.all ~filter ()) @@ fun books ->

    List.iter (fun book ->
        let slug = Book.slug book in
        let href =
          let%lwt slug = slug in
          Lwt.return (Router.path_of_controller (Router.Book slug) |> snd)
        in
        let link = Text.Link.create ~href ~text:(Book.title book) page in
        let li = Html.createLi (Page.document page) in
        Dom.appendChild li (Text.Link.root link);
        Dom.appendChild books_dom li)
      books
  in

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
