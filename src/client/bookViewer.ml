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
  table : Table.t;
}

let display_contents t contents =
  let rows =
    List.map
      (function
        | Book.Set (set, _parameters) ->
          (
            let slug = Set.slug set in
            let href =
              let%lwt slug = slug in
              Lwt.return (Router.path_of_controller (Router.Set slug) |> snd)
            in
            let cells =
              let open Lwt in
              [
                Table.Cell.text ~text:(Lwt.return "Set") t.page;
                Table.Cell.create ~content:(Formatters.Set.name_and_tunes set t.page) t.page;
                Table.Cell.text ~text:(Set.kind set >|= Kind.dance_to_string) t.page
              ]
            in
            Table.Row.create ~href ~cells t.page
          )

        | InlineSet (set, _parameters) ->
          (
            let cells =
              let open Lwt in [
                Table.Cell.text ~text:(Lwt.return "Set (inline)") t.page;
                Table.Cell.create ~content:(Formatters.Set.name_and_tunes set t.page) t.page;
                Table.Cell.text ~text:(Set.kind set >|= Kind.dance_to_string) t.page
              ]
            in
            Table.Row.create ~cells t.page
          )

        | Version (version, _parameters) ->
          (
            let slug = Version.slug version in
            let href =
              let%lwt slug = slug in
              Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
            in
            let cells =
              let open Lwt in [
                Table.Cell.text ~text:(Lwt.return "Version") t.page;
                Table.Cell.text ~text:(Version.tune version >>= Tune.name) t.page;
                Table.Cell.text ~text:(
                  let%lwt tune = Version.tune version in
                  let%lwt kind = Tune.kind tune in
                  let%lwt bars = Version.bars version in
                  let kind = (bars, kind) in
                  Lwt.return (Kind.version_to_string kind)
                ) t.page
              ]
            in
            Table.Row.create ~href ~cells t.page
          )
      )
      contents
    |> Lwt.return
  in
  let section = Table.Section.create ~rows t.page in
  Table.replace_bodies t.table (Lwt.return [section])

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let book = Book.get slug in
  let () =
    let title = Text.Heading.h2_static ~text:(Lwt.bind book Book.title) page in
    Dom.appendChild content (Text.Heading.root title);
    let subtitle = Text.Heading.h3_static ~text:(Lwt.bind book Book.subtitle) page in
    Dom.appendChild content (Text.Heading.root subtitle)
  in
  let date_text =
    let%lwt book = book in
    let%lwt date = Book.date book in
    if Date.is_none date then
      Lwt.return ""
    else
      Lwt.return (spf "Date: %s" (NesDate.to_string date))
  in
  let date = Text.Paragraph.create ~placeholder:"Getting date..." ~text:date_text page in
  Dom.appendChild content (Text.Paragraph.root date);

  let booklet_parameters =
    BookParameters.(
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
    BookParameters.(
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
  let b_parameters = BookParameters.make_instrument (Music.make_pitch B Flat (-1)) in
  let e_parameters = BookParameters.make_instrument (Music.make_pitch E Flat 0) in

  let c_pdf_href, b_pdf_href, e_pdf_href, bass_pdf_href,
      c_booklet_pdf_href, b_booklet_pdf_href, e_booklet_pdf_href, bass_booklet_pdf_href =
    Helpers.build_path ~api:true ~route:(Router.BookPdf slug)
      (),
    Helpers.build_path ~api:true ~route:(Router.BookPdf slug)
      ~query:["parameters", [
          b_parameters
          |> BookParameters.to_yojson |> Yojson.Safe.to_string
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.BookPdf slug)
      ~query:["parameters", [
          e_parameters
          |> BookParameters.to_yojson |> Yojson.Safe.to_string
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.BookPdf slug)
      ~query:["parameters", [
          bass_parameters
          |> BookParameters.to_yojson |> Yojson.Safe.to_string
        ]] (),
      Helpers.build_path ~api:true ~route:(Router.BookPdf slug)
      ~query:["parameters", [
          booklet_parameters
          |> BookParameters.to_yojson |> Yojson.Safe.to_string
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.BookPdf slug)
      ~query:["parameters", [
          BookParameters.(
            compose b_parameters booklet_parameters
            |> to_yojson |> Yojson.Safe.to_string
          )
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.BookPdf slug)
      ~query:["parameters", [
          BookParameters.(
            compose e_parameters booklet_parameters
            |> to_yojson |> Yojson.Safe.to_string
          )
        ]] (),
    Helpers.build_path ~api:true ~route:(Router.BookPdf slug)
      ~query:["parameters", [
          BookParameters.(
            compose bass_parameters booklet_parameters
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
  let prev_title = Text.Heading.h2_static ~text:(Lwt.return "Contents") page in
  Dom.appendChild content (Text.Heading.root prev_title);

  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~text:(Lwt.return "Type") page;
        Table.Cell.header_text ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
      ]
      page
  in
  let table = Table.create
      ~header
      ~kind:Table.Kind.Separated
      page
  in
  Dom.appendChild content (Table.root table);

  let t = {page; content; table} in
  Lwt.on_success book (fun prog -> Lwt.on_success (Book.contents prog) (display_contents t));
  t

let contents t =
  t.content

let init t =
  ignore t

let refresh t =
  ignore t
