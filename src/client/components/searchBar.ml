open Nes
open Js_of_ocaml
open Dancelor_client_html

(** Generic row showing an emoji on the left and a message on the right. *)
let emoji_row emoji message =
  tr [
    td [txt emoji];
    td ~a:[a_colspan 4] [txt message];
  ]

(** Abstraction of the possible states of the search bar. *)
type 'a search_bar_state =
  | StartTyping (** when the user has not typed anything yet *)
  | ContinueTyping (** when the user has not typed enough yet *)
  | NoResults (** when the search returned no results *)
  | Results of 'a list (** when the search returned results; guaranteed to be non empty; otherwise [NoResults] *)
  | Errors of string list (** when the search returned an error; guaranteed to be non empty *)

let make ~placeholder ~search ~make_result ~max_results ?on_enter () =
  let (search_text, set_search_text_immediately) = S.create "" in
  let (table_visible, set_table_visible) = S.create false in

  (* REVIEW: maybe this should become a generic signal helper to delay signals
     by a certain time? *)
  (** The following is a mechanism to only set the search text in a delayed
      fashion. The [search_text_setter] signal contains an Lwt promise whose job
      is to set the search text after a delayed time. The [set_search_text]
      function cancels the current Lwt promise (which does nothing if it already
      resolved) and creates a new one in its place. *)
  let (search_text_setter, set_search_text_setter) = S.create Lwt.return_unit in
  let set_search_text text =
    (* try cancelling the current search text setter *)
    Lwt.cancel (S.value search_text_setter);
    (* prepare the new search text setter *)
    let new_search_text_setter =
      (* FIXME: here, we need to delay by something like 100ms but [Lwt_unix]
         does not seem to be the answer. *)
      Lwt.pmsleep 0.30;%lwt
      set_search_text_immediately text;
      Lwt.return_unit
    in
    (* register it in the signal *)
    set_search_text_setter new_search_text_setter;
    (* fire it asynchronously *)
    Lwt.async (fun () -> new_search_text_setter)
  in

  (** Minimum number of characters for the search to fire. *)
  let min_characters = 3 in
  let min_characters_text = "three" in

  (** A signal that provides a [search_bar_state] view based on
      [search_text]. *)
  let search_bar_state =
    S.bind_s' search_text StartTyping @@ fun search_text ->
    if String.length search_text < min_characters then
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
              Option.value ~default:ignore on_enter (Js.to_string input##.value)
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
          S.bind_s' search_bar_state [] @@ function
          | StartTyping -> Lwt.return [emoji_row "👉" "Start typing to search."]
          | ContinueTyping -> Lwt.return [emoji_row "👉" (spf "Type at least %s characters." min_characters_text)]
          | NoResults -> Lwt.return [emoji_row "⚠️" "Your search returned no results."]
          | Errors errors -> Lwt.return @@ List.map (emoji_row "❌") errors
          | Results results ->
            let%lwt results = Lwt_list.map_p make_result results in
            Lwt.return @@
            if on_enter = None then
              results
            else
              results @ [emoji_row "👉" "Press enter for more results."]
        );
      ]
  ]
