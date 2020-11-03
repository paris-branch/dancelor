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
  let tune = Tune.get slug in

  let () =
    let title = Text.Heading.h2 ~text:(Lwt.bind tune Tune.name) page in
    Dom.appendChild content (Text.Heading.root title)
  in

  (* aka *)
  let () =
    let text = Formatters.Tune.aka_lwt tune in
    let aka = Text.Heading.h3 ~text page in
    Dom.appendChild content (Text.Heading.root aka)
  in

  (* recommended *)
  let () =
    let text = Formatters.Tune.recommended_lwt tune in
    let recommended = Text.Heading.h3 ~text page in
    Dom.appendChild content (Text.Heading.root recommended)
  in

  (* description *)
  let () =
    let text = Formatters.Tune.description_lwt tune in
    let description = Text.Heading.h3 ~text page in
    Dom.appendChild content (Text.Heading.root description)
  in

  Dom.appendChild content (Html.createHr document);

  (* Copied from VersionExplorer. Should be factorised. *)
  let header =
    Table.Row.create
      ~cells:[
        Table.Cell.header_text ~text:(Lwt.return "Disambiguation") page;
        Table.Cell.header_text ~text:(Lwt.return "Arranger") page;
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
      let%lwt tune = tune in
      VersionFilter.make ~tune:[tune] ()
    in
    let%lwt versions = Version.all ~filter () in

    (* If only one version, redirect directly to there. *)
    if List.length versions = 1 then
      (
        Lwt.async @@ fun () ->
        let%lwt href =
          let%lwt slug = Version.slug (List.hd versions) in
          Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
        in
        Html.window##.location##.href := js href;
        Lwt.return_unit
      );

    Lwt.return (List.map (fun version ->
        let href =
          let%lwt slug = Version.slug version in
          Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
        in
        let cells =
          let tune = Version.tune version in
          let open Lwt in [
            Table.Cell.text ~text:(Version.disambiguation version) page;
            Table.Cell.text ~text:(Version.arranger version >>= Formatters.Credit.line) page;
            Table.Cell.text ~text:(tune >>= Formatters.Kind.full_string version) page;
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
