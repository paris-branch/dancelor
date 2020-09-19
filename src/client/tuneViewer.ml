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
  let group = Tune.get slug in
  let title = Text.Heading.h1 ~text:(Lwt.bind group Tune.name) page in
  Dom.appendChild content (Text.Heading.root title);
  let aka_text, kind_text, author_text =
    let open Lwt in
    (match%lwt group >>= Tune.alt_names with
     | [] -> Lwt.return ""
     | names -> Printf.sprintf "Also known as: %s" (String.concat ", " names) |> Lwt.return),
    (let%lwt kind = group >>= Tune.kind in
     Lwt.return ("Kind: " ^ Kind.base_to_string kind)),
    (group >>= Tune.author >>= Formatters.Credit.line >|= Printf.sprintf "Author: %s")
  in
  let aka, kind, author =
    Text.Paragraph.create ~placeholder:"Also known as:" ~text:aka_text page,
    Text.Paragraph.create ~placeholder:"Kind:" ~text:kind_text page,
    Text.Paragraph.create ~placeholder:"Author: " ~text:author_text page
  in
  Dom.appendChild content (Text.Paragraph.root aka);
  Dom.appendChild content (Text.Paragraph.root kind);
  Dom.appendChild content (Text.Paragraph.root author);
  Dom.appendChild content (Html.createHr document);

  let lyversions =
    Text.Paragraph.create ~text:(Lwt.return "Versions:") page
  in
  Dom.appendChild content (Text.Paragraph.root lyversions);

  (* Copied from VersionExplorer. Should be factorised. *)
  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Key") page;
        Table.Cell.header_text ~text:(Lwt.return "Structure") page
      ]
      page
  in
  let table = Table.create ~kind:Table.Kind.Separated ~header page in
  Dom.appendChild content (Table.root table);

  (* Copied from VersionExplorer. Should be factorised. *)
  let rows =
    let%lwt filter =
      let%lwt group = group in
      VersionFilter.make ~group:[group] ()
    in
    let%lwt versions = Version.all ~filter () in
    Lwt.return (List.map (fun version ->
        let href =
          let%lwt slug = Version.slug version in
          Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
        in
        let cells =
          let group = Version.group version in
          let open Lwt in [
            Table.Cell.text ~text:(group >>= Formatters.Kind.full_string version) page;
            Table.Cell.text ~text:(Version.key version >|= Music.key_to_pretty_string) page;
            Table.Cell.text ~text:(Version.structure version) page;
          ]
        in
        Table.Row.create ~href ~cells page) versions)
  in
  let section = Table.Section.create ~rows page in
  Table.replace_bodies table (Lwt.return [section]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
