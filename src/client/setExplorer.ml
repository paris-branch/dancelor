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

let create page = 
  let document = Page.document page in
  let content = Html.createDiv document in
  let title = Html.createH1 document in
  title##.textContent := Js.some (js "All Sets");
  Dom.appendChild content title;
  Dom.appendChild content (Html.createHr document);
  Dom.appendChild content (Html.createBr document);
  let header = 
    Table.Row.create 
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~alt:(Lwt.return "Sets") ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Deviser") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Actions") page]
      page
  in
  let rows = 
    Set.get_all ()
    |> NesLwtList.sort (fun s1 s2 -> 
      let%lwt s1slug = Set.slug s1 and s2slug = Set.slug s2 in
      Lwt.return (compare s1slug s2slug))
    |> NesLwtList.map (fun set -> 
      let href = 
        let%lwt slug = Set.slug set in
        Lwt.return (Router.path_of_controller (Router.Set slug) |> snd) 
      in
      let cells = 
        let open Lwt in [
        Table.Cell.link ~href ~text:(Set.name set) page;
        Table.Cell.text ~text:(Set.deviser set >>= Formatters.Credit.line) page;
        Table.Cell.text ~text:(Set.kind set >|= Kind.dance_to_string) page;
        Table.Cell.text ~text:(Lwt.return "") page]
      in
      Table.Row.create ~href ~cells page)
  in
  let section = Table.Section.create ~rows page in
  let table = Table.create
    ~header
    ~kind:Table.Kind.Separated
    ~contents:(Lwt.return [section])
    page
  in
  Dom.appendChild content (Table.root table);
  {page; content}

let contents t =
  t.content

let refresh t = 
  ignore t

let init t = 
  ignore t
