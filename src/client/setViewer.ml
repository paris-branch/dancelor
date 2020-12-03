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
  versions : Html.uListElement Js.t;
}

let display_versions_and_parameters t versions_and_parameters =
  t.versions##.textContent := Js.null;
  List.iter (fun (version, _parameters) ->
      (* FIXME: use parameters *)
    let tune = Version.tune version in
    let slug = Version.slug version in
    let href =
      let%lwt slug = slug in
      Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
    in
    let name = Lwt.bind tune Tune.name in
    let title = Text.Link.create ~href ~text:name t.page in
    let source =
      Lwt.map (fun slug ->
        spf "/%s%s"
          Constant.api_prefix
          (Router.path_of_controller (Router.VersionSvg slug) |> snd))
        slug
    in
    let img = Image.create ~source t.page in
    let li = Html.createLi (Page.document t.page) in
    Dom.appendChild li (Text.Link.root title);
    Dom.appendChild li (Image.root img);
    Dom.appendChild t.versions li)
    versions_and_parameters

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let set = Set.get slug in

  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.bind set Set.name) page in
    Dom.appendChild content (Text.Heading.root title)
  in

  let () =
    let text = Formatters.Set.works_lwt set in
    let works = Text.Heading.h3_static ~text page in
    Dom.appendChild content (Text.Heading.root works)
  in

  let () =
    let open Lwt in
    let text = set >>= Set.kind >|= Kind.dance_to_pretty_string in
    Text.Heading.h3_static ~text page
    |> Text.Heading.root
    |> Dom.appendChild content
  in

  let () =
    let open Lwt in
    let line_block =
      match%lwt set >>= Set.deviser with
      | None -> Lwt.return []
      | Some deviser ->
        let%lwt line_block = Formatters.Credit.line (Some deviser) page in
        Lwt.return ((Formatters.text "Set devised by" page :> Dom.node Js.t) :: line_block)
    in
    Text.Heading.h3 ~content:line_block page
    |> Text.Heading.root
    |> Dom.appendChild content
  in

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

  Dom.appendChild content (Inputs.Button.root c_pdf);
  Dom.appendChild content (Inputs.Button.root b_pdf);
  Dom.appendChild content (Inputs.Button.root e_pdf);
  Dom.appendChild content (Inputs.Button.root bass_pdf);
  Dom.appendChild content (Html.createBr document);
  Dom.appendChild content (Inputs.Button.root ly);
  Dom.appendChild content (Html.createHr document);

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

  let versions = Html.createUl (Page.document page) in
  versions##.textContent := Js.some (js "Loading versions...");
  Dom.appendChild content versions;
  let t = {page; content; versions} in
  Lwt.on_success set (fun set -> Lwt.on_success (Set.versions_and_parameters set) (display_versions_and_parameters t));
  t

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
