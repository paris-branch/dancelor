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
  title##.textContent := Js.some (js "All Tunes");
  Dom.appendChild content title;
  Dom.appendChild content (Html.createHr document);
  Dom.appendChild content (Html.createBr document);
  let search_div = Html.createDiv document in
  let key_section_header = Html.createB document in
  key_section_header##.textContent := Js.some (js "Filter by key:");
  Dom.appendChild search_div key_section_header;
  let line1 = Html.createDiv document in
  List.iter (fun text ->
    let id = Printf.sprintf "button_%s" text in
    let b = Buttons.Toggle.create ~id ~text ~on_change:(fun _ -> ()) page in
    Style.set ~width:"3rem" ~margin:"0pt 2pt 2pt 0pt" (Buttons.Toggle.root b);
    Dom.appendChild line1 (Buttons.Toggle.root b))
    ["C"; "G"; "D"; "A"; "E"; "B"; "F#"; "C#"; "F"; "Bb"; "Eb"; "Ab"; "Db"];
  Dom.appendChild search_div line1;
  let line2 = Html.createDiv document in
  List.iter (fun text ->
    let id = Printf.sprintf "button_%s" text in
    let b = Buttons.Toggle.create ~id ~text ~on_change:(fun _ -> ()) page in
    Style.set ~width:"3rem" ~margin:"0pt 2pt 2pt 0pt" (Buttons.Toggle.root b);
    Dom.appendChild line2 (Buttons.Toggle.root b))
    ["Am"; "Em"; "Bm"; "F#m"; "C#m"; "G#m"; "D#m"; "A#m"; "Dm"; "Gm"; "Cm"; "Fm"; "Bbm"];
  Dom.appendChild search_div line2;
  Dom.appendChild search_div (Html.createBr document);
  let kind_section_header = Html.createB document in
  kind_section_header##.textContent := Js.some (js "Filter by kind:");
  Dom.appendChild search_div kind_section_header;
  let kinds = Html.createDiv document in
  List.iter (fun text ->
    let id = Printf.sprintf "button_%s" text in
    let b = Buttons.Toggle.create ~id ~text ~on_change:(fun _ -> ()) page in
    Style.set ~width:"6rem" ~margin:"0pt 2pt 2pt 0pt" (Buttons.Toggle.root b);
    Dom.appendChild kinds (Buttons.Toggle.root b))
    ["Reel"; "Strathspey"; "Jig"; "Waltz"];
  Dom.appendChild search_div kinds;
  Dom.appendChild search_div (Html.createBr document);
  let bars_section_header = Html.createB document in
  bars_section_header##.textContent := Js.some (js "Filter by length:");
  Dom.appendChild search_div bars_section_header;
  let bars = Html.createDiv document in
  List.iter (fun text ->
    let id = Printf.sprintf "button_%s" text in
    let b = Buttons.Toggle.create ~id ~text ~on_change:(fun _ -> ()) page in
    Style.set ~width:"6rem" ~margin:"0pt 2pt 2pt 0pt" (Buttons.Toggle.root b);
    Dom.appendChild bars (Buttons.Toggle.root b))
    ["32 Bars"; "40 Bars"; "48 Bars"; "64 Bars"];
  Dom.appendChild search_div bars;
  Dom.appendChild content search_div;
  Dom.appendChild content (Html.createBr document);
  let header = 
    Table.Row.create 
      ~cells:[
        Table.Cell.header_text ~width:"45%" ~alt:(Lwt.return "Tunes") ~text:(Lwt.return "Name") page;
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
      let href = 
        let%lwt slug = Tune.slug tune in
        Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd) 
      in
      let cells = 
        let group = Tune.group tune in 
        let open Lwt in [
        Table.Cell.link ~href ~text:(group >>= TuneGroup.name) page;
        Table.Cell.text ~text:(group >>= Formatters.Kind.full_string tune) page;
        Table.Cell.text ~text:(Tune.key tune >|= Music.key_to_string) page;
        Table.Cell.text ~text:(Tune.structure tune) page;
        Table.Cell.text ~text:(group >>= TuneGroup.author >>= Formatters.Credit.line) page]
      in
      Table.Row.create ~href ~cells page) tunes)
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
