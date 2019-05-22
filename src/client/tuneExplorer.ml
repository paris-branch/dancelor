open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model

module Html = Dom_html

let js = Js.string

type t = 
{
  page : Page.t;
  content : Html.divElement Js.t;
}

module Kind = struct

  open Kind

  let full_string tune group = 
    let open Lwt in
    let%lwt base = TuneGroup.kind group >|= Kind.base_to_char in
    let%lwt bars = Tune.bars tune in
    Lwt.return (Printf.sprintf "%i%c" bars base)

end

module Credit = struct
  
  open Credit

  let line = function
    | None -> Lwt.return ""
    | Some c -> line c

end

let create page = 
  let document = Page.document page in
  let content = Html.createDiv document in
  let title = Html.createH1 document in
  title##.textContent := Js.some (js "All Tunes");
  Dom.appendChild content title;
  let header = 
    Table.Row.create 
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~text:(Lwt.return "Name") page;
        Table.Cell.header_text ~text:(Lwt.return "Kind") page;
        Table.Cell.header_text ~text:(Lwt.return "Key") page;
        Table.Cell.header_text ~text:(Lwt.return "Structure") page;
        Table.Cell.header_text ~width:"30%" ~text:(Lwt.return "Author") page] 
      page
  in
  let rows = 
    let%lwt tunes = Tune.get_all () in
    Lwt.return (List.map (fun score -> 
      let tune = Score.value score in
      let cells = 
        let group = Tune.group tune in 
        let open Lwt in [
        Table.Cell.text ~text:(group >>= TuneGroup.name) page;
        Table.Cell.text ~text:(group >>= Kind.full_string tune) page;
        Table.Cell.text ~text:(Tune.key tune >|= Music.key_to_string) page;
        Table.Cell.text ~text:(Tune.structure tune) page;
        Table.Cell.text ~text:(group >>= TuneGroup.author >>= Credit.line) page]
      in
      Table.Row.create ~cells page) tunes)
  in
  let table = Table.create
    ~header
    ~contents:rows
    page
  in
  Dom.appendChild content (Table.root table);
  {page; content}

let contents t =
  t.content
