open Nes
open Js_of_ocaml
open Dancelor_client_html

(** Row for when the search returned no results. *)
let no_results_row =
  tr [
    td [txt "⚠️"];
    td [txt "Your search returned no results."];
  ]

(** Rows for when the search returned error messages. *)
let error_rows messages =
  Fun.flip List.map messages @@ fun message ->
  tr [
    td [txt "❌"];
    td [txt message];
  ]

let make ~placeholder ~search ~make_result ~max_results ~on_enter =
  let (search_text, set_search_text) = S.create "" in
  let (table_visible, set_table_visible) = S.create false in

  div [
    input ~a:[
      a_input_type `Text;
      a_placeholder placeholder;
      a_oninput (fun event ->
          (
            Js.Opt.iter event##.target @@ fun elt ->
            Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
            set_search_text (Js.to_string input##.value)
          );
          false
        );
      a_autofocus ();
      a_onfocus (fun _ -> set_table_visible true; false);
      a_onblur (fun _ -> set_table_visible false; false);
      a_onkeyup (fun event ->
          if Js.Optdef.to_option event##.key = Some (Js.string "Enter") then
            (
              Js.Opt.iter event##.target @@ fun elt ->
              Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
              on_enter (Js.to_string input##.value)
            );
          true
        );
    ] ();

    tablex
      ~a:[
        R.a_class (
          Fun.flip S.map table_visible @@ function
          | false -> ["dropdown-table"]
          | true -> ["dropdown-table"; "visible"]
        );
      ]
      [
        R.tbody (
          S.bind search_text @@ fun search_text ->
          S.from' [] @@
          if String.length search_text < 3 then
            (
              let message =
                if search_text = ""
                then "Start typing to search."
                else "Type at least three characters."
              in
              Lwt.return [
                tr [
                  td [txt "👉"];
                  td [txt message];
                ]
              ]
            )
          else
            Fun.flip Lwt.map (search search_text) @@ function
            | Error messages -> error_rows messages
            | Ok [] -> [no_results_row]
            | Ok results ->
              List.map make_result (List.sub max_results results)
              @ [tr [td [txt "👉"]; td ~a:[a_colspan 4] [txt "Press enter for more results."]]]
        );
      ]
  ]
