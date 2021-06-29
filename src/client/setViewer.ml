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
  versions : Html.uListElement Js.t;
}

let display_versions_and_parameters t versions_and_parameters =
  t.versions##.textContent := Js.null;
  List.iter
    (fun (version, _parameters) ->
       (* FIXME: use parameters *)
       let tune = Version.tune version in
       let slug = Version.slug version in
       let href =
         let%lwt slug = slug in
         Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
       in
       let name = Text.Link.create ~href ~text:(tune >>=| Tune.name) t.page in
       let title = Html.createH4 (Page.document t.page) in
       Dom.appendChild title (Text.Link.root name);

       let source =
         Lwt.map (fun slug ->
             spf "/%s%s"
               Constant.api_prefix
               (Router.path_of_controller (Router.VersionSvg slug) |> snd))
           slug
       in
       let img = Image.create ~source t.page in
       Dom.appendChild t.versions title;
       Dom.appendChild t.versions (Image.root img))
    versions_and_parameters

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let set = Set.get slug in

  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.bind set Set.name) page in
    let title = Text.Heading.root title in
    title##.classList##add (js "title");
    Dom.appendChild content title
  in

  let () =
    let text = Formatters.Set.works_lwt set in
    let works = Text.Heading.h3_static ~text page in
    let works = Text.Heading.root works in
    works##.classList##add (js "title");
    Dom.appendChild content works
  in

  let () =
    let open Lwt in
    let text = set >>= Set.kind >|= Kind.dance_to_pretty_string in
    let kind = Text.Heading.h3_static ~text page in
    let kind = Text.Heading.root kind in
    kind##.classList##add (js "title");
    Dom.appendChild content kind
  in

  let () =
    let open Lwt in
    let line_block =
      match%lwt set >>= Set.deviser with
      | None -> Lwt.return []
      | Some deviser ->
        let%lwt line_block = Formatters.Credit.line (Some deviser) page in
        Lwt.return ((Formatters.text "Set devised by " page :> Dom.node Js.t) :: line_block)
    in
    let by = Text.Heading.h3 ~content:line_block page in
    let by = Text.Heading.root by in
    by##.classList##add (js "title");
    Dom.appendChild content by
  in

  (* Buttons *)

  let () =

    let bass_parameters =
      SetParameters.(
        make ~every_version:VersionParameters.(
            make
              ~clef:Music.Bass
              ~transposition:(Relative(Music.pitch_c, Music.make_pitch C Natural (-1)))
              ()
          )
          ()
      )
    in
    let b_parameters = SetParameters.make_instrument (Music.make_pitch B Flat (-1)) in
    let e_parameters = SetParameters.make_instrument (Music.make_pitch E Flat 0) in

    let c_pdf_href, b_pdf_href, e_pdf_href, bass_pdf_href,
        ly_href
      =
      Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
        (),
      Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
        ~query:["parameters", [
            b_parameters
            |> SetParameters.to_yojson |> Yojson.Safe.to_string
          ]] (),
      Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
        ~query:["parameters", [
            e_parameters
            |> SetParameters.to_yojson |> Yojson.Safe.to_string
          ]] (),
      Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
        ~query:["parameters", [
            bass_parameters
            |> SetParameters.to_yojson |> Yojson.Safe.to_string
          ]] (),
      Helpers.build_path ~api:true ~route:(Router.SetLy slug) ()
    in
    let c_pdf, b_pdf, e_pdf, bass_pdf,
        ly
      =
      Inputs.Button.create ~href:(Lwt.return c_pdf_href) ~icon:"file-pdf" ~text:"PDF" page,
      Inputs.Button.create ~href:(Lwt.return b_pdf_href) ~icon:"file-pdf" ~text:"PDF (B♭)" page,
      Inputs.Button.create ~href:(Lwt.return e_pdf_href) ~icon:"file-pdf" ~text:"PDF (E♭)" page,
      Inputs.Button.create ~href:(Lwt.return bass_pdf_href) ~icon:"file-pdf" ~text:"PDF (𝄢)" page,
      Inputs.Button.create ~href:(Lwt.return ly_href) ~icon:"file-alt" ~text:"LilyPond" page
    in

    let div = Html.createDiv (Page.document page) in
    div##.classList##add (js "buttons");
    Dom.appendChild content div;

    Dom.appendChild div (Inputs.Button.root c_pdf);
    Dom.appendChild div (Inputs.Button.root b_pdf);
    Dom.appendChild div (Inputs.Button.root e_pdf);
    Dom.appendChild div (Inputs.Button.root bass_pdf);
    Dom.appendChild div (Html.createBr document);
    Dom.appendChild div (Inputs.Button.root ly);
  in

  let () =
    let open Lwt in
    let text =
      match%lwt set >>= Set.instructions with
      | "" -> Lwt.return ""
      | instructions -> Lwt.return ("Instructions: " ^ instructions)
    in
    Text.Paragraph.create ~text page
    |> Text.Paragraph.root
    |> Dom.appendChild content
  in

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Previsualisation") page in
    Dom.appendChild content (Text.Heading.root pretext);
  in

  let versions = Html.createUl (Page.document page) in
  versions##.textContent := Js.some (js "Loading tunes...");
  Dom.appendChild content versions;

  let t = {page; content; versions} in
  Lwt.on_success set (fun set -> Lwt.on_success (Set.versions_and_parameters set) (display_versions_and_parameters t));

  (* Books in which this version can be found *)

  let () =
    let pretext = Text.Heading.h3_static ~text:(Lwt.return "Books in Which This Set Appears") page in
    Dom.appendChild content (Text.Heading.root pretext);

    let tableHolder = Html.createDiv (Page.document page) in
    Dom.appendChild content tableHolder;

    let books_lwt =
      let%lwt set = set in
      let filter = Formula.pred (Book.Filter.ExistsSet (Set.Filter.Is set)) in
      Book.all ~filter ()
    in

    let table = Dancelor_client_tables.Book.make books_lwt page in

    (* When getting the books, decide to show just a text or the table *)

    Lwt.on_success books_lwt @@ fun books ->
    if books = [] then
      let text = Text.Paragraph.create ~text:(Lwt.return "There are no books containing this version.") page in
      Dom.appendChild tableHolder (Text.Paragraph.root text)
    else
      Dom.appendChild tableHolder (Table.root table)
  in

  t

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
