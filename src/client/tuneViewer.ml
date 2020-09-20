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
    let aka_text =
      match%lwt Lwt.bind tune Tune.alt_names with
      | [] -> Lwt.return ""
      | names -> Printf.sprintf "Also known as: %s" (String.concat ", " names) |> Lwt.return
    in
    let aka = Text.Heading.h3 ~text:aka_text page in
    Dom.appendChild content (Text.Heading.root aka)
  in

  (* kind and author *)
  let () =
    let open Lwt in
    let kind_by_author_text =
      let%lwt kind = tune >>= Tune.kind in
      let kind = Kind.base_to_string kind in
      let%lwt author = tune >>= Tune.author in
      match author with
      | None ->
        Lwt.return (String.capitalize_ascii kind)
      | Some author when Credit.is_trad author ->
        Lwt.return ("Traditional " ^ kind)
      | Some author ->
        let%lwt line = Credit.line author in
        Lwt.return (String.capitalize_ascii kind ^ " by " ^ line)
    in
    let kind_by_author = Text.Heading.h3 ~text:kind_by_author_text page in
    Dom.appendChild content (Text.Heading.root kind_by_author)
  in

  Dom.appendChild content (Html.createHr document);

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
      let%lwt group = tune in
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
