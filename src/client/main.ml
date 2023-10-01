open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_html

module Html = Dom_html

let js = Js.string

(* Whether to show the menu or not. [None] indicates the default (yes on
   desktop, no on mobile). *)
let (show_menu, set_show_menu) = React.S.create None

let header =
  div ~a:[a_class ["content"]] [

    (* Toggle for smartphone devices. *)
    a
      ~a:[
        a_id "to_nav";
        a_onclick (fun _ ->
            set_show_menu % Option.some % not @@ (S.value show_menu = Some true);
            false
          )
      ]
      [i ~a:[a_class ["fas"; "fa-bars"]] []];

    (* A glorious title. *)
    a ~a:[a_href "/"] [h1 [txt "Dancelor"]];

    (* Navigation menu. *)
    ul
      ~a:[
        a_id "nav";
        R.a_style (
          Fun.flip S.map show_menu @@ function
          | None -> ""
          | Some true -> "display: block;"
          | Some false -> "display: none;"
        )
      ]
      [
        li [a ~a:[a_href PageRouter.(path (Search None))] [txt "Search"]];

        li [
          txt "Explore ▾";
          ul ~a:[a_class ["subnav"]] [
            li [a ~a:[a_href PageRouter.(path VersionAll)] [txt "Tunes"]];
            li [a ~a:[a_href PageRouter.(path SetAll)] [txt "Sets"]];
            li [a ~a:[a_href PageRouter.(path BookAll)] [txt "Books"]];
          ]
        ];

        li [
          txt "Add ▾";
          ul ~a:[a_class ["subnav"]] [
            li [a ~a:[a_href PageRouter.(path VersionAdd)] [txt "Tune"]];
            li [a ~a:[a_href PageRouter.(path SetCompose)] [txt "Set"]];
            li [a ~a:[a_href PageRouter.(path BookCompose)] [txt "Book"]];
          ]
        ]
      ]
  ]

let on_load _ev =
  let page = Page.create () in
  Page.set_header page (To_dom.of_div header);
  let url =
    Html.window##.location##.href
    |> Js.to_string
    |> Uri.of_string
  in
  let module M = (val (Dispatcher.dispatch url) : Page.CONTENTS) in
  let contents = M.create page in
  Page.set_contents (module M) page contents;
  M.init contents;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
