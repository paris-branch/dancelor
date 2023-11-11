open Nes
open Js_of_ocaml
open Dancelor_client_html

(** Row for when the search returned no results. *)
let no_results_row =
  tr [
    td [txt "⚠️"];
    td [txt "Your search returned no results."];
  ]

(** Row for when the user has not even started typing. *)
let start_typing_row =
  tr [
    td [txt "👉"];
    td [txt "Start typing to search."];
  ]

(** Row for when the user started typing but did not give enough characters to
    trigger a meaningful search. *)
let continue_typing_row =
  tr [
    td [txt "👉"];
    td [txt "Type at least three characters."];
  ]

(** Rows for when the search returned error messages. *)
let error_rows messages =
  Fun.flip List.map messages @@ fun message ->
  tr [
    td [txt "❌"];
    td [txt message];
  ]

(** Abstraction of the possible states of the search bar. *)
type 'a search_bar_state =
  | StartTyping (** when the user has not typed anything yet *)
  | ContinueTyping (** when the user has not typed enough yet *)
  | NoResults (** when the search returned no results *)
  | Results of 'a list (** when the search returned results; guaranteed to be non empty; otherwise [NoResults] *)
  | Errors of string list (** when the search returned an error; guaranteed to be non empty *)

let make ~placeholder ~search ~make_result ~max_results ~on_enter =
  let (search_text, set_search_text) = S.create "" in
  let (table_visible, set_table_visible) = S.create false in

  (** A signal that provides a [search_bar_state] view based on
      [search_text]. *)
  let search_bar_state =
    S.bind_s' search_text StartTyping @@ fun search_text ->
    if String.length search_text < 3 then
      (
        Lwt.return @@
        if search_text = "" then
          StartTyping
        else
          ContinueTyping
      )
    else
      Fun.flip Lwt.map (search search_text) @@ function
      | Error messages -> Errors messages
      | Ok [] -> NoResults
      | Ok results -> Results (List.sub max_results results)
  in

  div ~a:[a_class ["search-bar"]] [
    div ~a:[
      R.a_class (
        Fun.flip S.map table_visible @@ function
        | false -> []
        | true -> ["visible"]
      );
      a_onclick (fun _ -> set_table_visible false; false);
    ] [];

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
          Fun.flip S.map search_bar_state @@ function
          | StartTyping -> [start_typing_row]
          | ContinueTyping -> [continue_typing_row]
          | NoResults -> [no_results_row]
          | Results results -> List.map make_result results @ [tr [td [txt "👉"]; td ~a:[a_colspan 4] [txt "Press enter for more results."]]]
          | Errors errors -> error_rows errors
        );
      ]
  ]
