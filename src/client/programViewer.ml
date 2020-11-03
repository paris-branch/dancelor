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
  sets : Html.uListElement Js.t;
}

let display_sets_and_parameters t sets_and_parameters =
  t.sets##.textContent := Js.null;
  List.iter (fun (set, _parameters) ->
      (* FIXME: use parameters *)
    let slug = Set.slug set in
    let href =
      let%lwt slug = slug in
      Lwt.return (Router.path_of_controller (Router.Set slug) |> snd)
    in
    let name = Set.name set in
    let link = Text.Link.create ~href ~text:name t.page in
    let li = Html.createLi (Page.document t.page) in
    Dom.appendChild li (Text.Link.root link);
    Dom.appendChild t.sets li)
    sets_and_parameters

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let program = Program.get slug in
  let title = Text.Heading.h2 ~text:(Lwt.bind program Program.name) page in
  Dom.appendChild content (Text.Heading.root title);
  let date_text =
    let open Lwt in
    (program >>= Program.date >|= NesDate.to_string >|= spf "Date: %s")
  in
  let date = Text.Paragraph.create ~placeholder:"Date:" ~text:date_text page in
  Dom.appendChild content (Text.Paragraph.root date);

  let booklet_parameters =
    ProgramParameters.(
      make
        ~front_page:true
        ~table_of_contents:End
        ~two_sided:true
        ~every_set:SetParameters.(
            make
              ~forced_pages:2
              ()
          )
        ()
    )
  in
  let bass_parameters =
    ProgramParameters.(
      make ~every_set:SetParameters.(
          make ~every_version:VersionParameters.(
              make
                ~clef:Music.Bass
                ~transposition:(Relative(Music.pitch_c, Music.make_pitch C Natural (-1)))
                ()
            )
            ()
        )
        ()
    )
  in
  let b_parameters = ProgramParameters.make_instrument (Music.make_pitch B Flat (-1)) in
  let e_parameters = ProgramParameters.make_instrument (Music.make_pitch E Flat 0) in

  let c_pdf_href, b_pdf_href, e_pdf_href, bass_pdf_href,
      c_booklet_pdf_href, b_booklet_pdf_href, e_booklet_pdf_href, bass_booklet_pdf_href =
    Helpers.build_path ~api:true ~route:(Router.ProgramPdf slug)
      (),
    Helpers.build_path ~api:true ~route:(Router.ProgramPdf slug)
      ~query:["parameters", [
          b_parameters
          |> ProgramParameters.to_yojson |> Yojson.Safe.to_string
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.ProgramPdf slug)
      ~query:["parameters", [
          e_parameters
          |> ProgramParameters.to_yojson |> Yojson.Safe.to_string
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.ProgramPdf slug)
      ~query:["parameters", [
          bass_parameters
          |> ProgramParameters.to_yojson |> Yojson.Safe.to_string
        ]] (),
      Helpers.build_path ~api:true ~route:(Router.ProgramPdf slug)
      ~query:["parameters", [
          booklet_parameters
          |> ProgramParameters.to_yojson |> Yojson.Safe.to_string
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.ProgramPdf slug)
      ~query:["parameters", [
          ProgramParameters.(
            compose ~parent:b_parameters booklet_parameters
            |> to_yojson |> Yojson.Safe.to_string
          )
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.ProgramPdf slug)
      ~query:["parameters", [
          ProgramParameters.(
            compose ~parent:e_parameters booklet_parameters
            |> to_yojson |> Yojson.Safe.to_string
          )
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.ProgramPdf slug)
      ~query:["parameters", [
          ProgramParameters.(
            compose ~parent:bass_parameters booklet_parameters
            |> to_yojson |> Yojson.Safe.to_string
          )
        ]] ()
  in
  let c_pdf, b_pdf, e_pdf, bass_pdf,
      c_booklet_pdf, b_booklet_pdf, e_booklet_pdf, bass_booklet_pdf =
    Inputs.Button.create ~href:(Lwt.return c_pdf_href) ~icon:"file-pdf" ~text:"PDF" page,
    Inputs.Button.create ~href:(Lwt.return b_pdf_href) ~icon:"file-pdf" ~text:"PDF (B♭)" page,
    Inputs.Button.create ~href:(Lwt.return e_pdf_href) ~icon:"file-pdf" ~text:"PDF (E♭)" page,
    Inputs.Button.create ~href:(Lwt.return bass_pdf_href) ~icon:"file-pdf" ~text:"PDF (𝄢)" page,
    Inputs.Button.create ~href:(Lwt.return c_booklet_pdf_href) ~icon:"file-pdf" ~text:"PDF (book)" page,
    Inputs.Button.create ~href:(Lwt.return b_booklet_pdf_href) ~icon:"file-pdf" ~text:"PDF (B♭, book)" page,
    Inputs.Button.create ~href:(Lwt.return e_booklet_pdf_href) ~icon:"file-pdf" ~text:"PDF (E♭, book)" page,
    Inputs.Button.create ~href:(Lwt.return bass_booklet_pdf_href) ~icon:"file-pdf" ~text:"PDF (𝄢, book)" page
  in
  Dom.appendChild content (Inputs.Button.root c_pdf);
  Dom.appendChild content (Inputs.Button.root b_pdf);
  Dom.appendChild content (Inputs.Button.root e_pdf);
  Dom.appendChild content (Inputs.Button.root bass_pdf);
  Dom.appendChild content (Html.createBr document);
  Dom.appendChild content (Inputs.Button.root c_booklet_pdf);
  Dom.appendChild content (Inputs.Button.root b_booklet_pdf);
  Dom.appendChild content (Inputs.Button.root e_booklet_pdf);
  Dom.appendChild content (Inputs.Button.root bass_booklet_pdf);
  Dom.appendChild content (Html.createHr document);
  let prev_title = Text.Heading.h2 ~text:(Lwt.return "Sets") page in
  Dom.appendChild content (Text.Heading.root prev_title);
  let sets = Html.createUl (Page.document page) in
  sets##.textContent := Js.some (js "Loading sets...");
  Dom.appendChild content sets;
  let t = {page; content; sets} in
  Lwt.on_success program (fun prog -> Lwt.on_success (Program.sets_and_parameters prog) (display_sets_and_parameters t));
  t

let contents t =
  t.content

let init t =
  ignore t

let refresh t =
  ignore t
