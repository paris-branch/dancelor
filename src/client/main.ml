open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_html

module Html = Dom_html

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
      [i ~a:[a_class ["material-symbols-outlined"]] [txt "menu"]];

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
        li [
          a ~a:[a_href PageRouter.(path (Explore None))] [
            i ~a:[a_class ["material-symbols-outlined"]] [txt "search"];
            txt " Explore";
          ];
        ];

        li [
          txt "Add ";
          i ~a:[a_class ["material-symbols-outlined"]] [txt "arrow_drop_down"];

          ul ~a:[a_class ["subnav"]] [
            li [
              a ~a:[a_href PageRouter.(path PersonAdd)] [
                i ~a:[a_class ["material-symbols-outlined"]] [txt "person"];
                txt " Person";
              ]
            ];
            li [
              a ~a:[a_href PageRouter.(path DanceAdd)] [
                i ~a:[a_class ["material-symbols-outlined"]] [txt "directions_walk"];
                txt " Dance";
              ]
            ];
            li [
              a ~a:[a_href PageRouter.(path TuneAdd)] [
                i ~a:[a_class ["material-symbols-outlined"]] [txt "music_note"];
                txt " Tune";
              ];
            ];
            li [
              a ~a:[a_href PageRouter.(path VersionAdd)] [
                i ~a:[a_class ["material-symbols-outlined"]] [txt "music_note"];
                txt " Version";
              ];
            ];
            li [
              a ~a:[a_href PageRouter.(path SetCompose)] [
                i ~a:[a_class ["material-symbols-outlined"]] [txt "format_list_bulleted"];
                txt " Set";
              ];
            ];
            li [
              a ~a:[a_href PageRouter.(path BookCompose)] [
                i ~a:[a_class ["material-symbols-outlined"]] [txt "library_books"];
                txt " Book";
              ];
            ];
          ]
        ]
      ]
  ]

let on_load _ev =
  let page = Dancelor_client_elements.Page.create () in
  Dancelor_client_elements.Page.set_header page (To_dom.of_div header);
  let url =
    Html.window##.location##.href
    |> Js.to_string
    |> Uri.of_string
  in
  let module M = (val (Dispatcher.dispatch url) : Dancelor_client_elements.Page.CONTENTS) in
  let contents = M.create page in
  Dancelor_client_elements.Page.set_contents (module M) page contents;
  M.init contents;
  Js._false

let _ =
  Html.window##.onload := Html.handler on_load
