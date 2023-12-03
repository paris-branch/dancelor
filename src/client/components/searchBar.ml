open Nes
open Js_of_ocaml
open Dancelor_client_html

(** Generic row showing an emoji on the left and a message on the right. *)
let emoji_row emoji message =
  tr [
    td [txt emoji];
    td ~a:[a_colspan 4] [txt message];
  ]

type 'result state =
  | StartTyping
  | ContinueTyping
  | NoResults
  | Results of 'result list
  | Errors of string list

type ('result, 'html) t = {
  html : ([> Html_types.input] as 'html) elt;
  state : 'result state S.t;
  set_text : (string -> unit);
}

(** Minimum number of characters for a search bar to fire. *)
let min_characters = 3

let make ~placeholder ~search ?on_focus ?on_enter ?(autofocus=false) () =
  (** A signal containing the search text. *)
  let (text, set_text_immediately) = S.create "" in

  (* REVIEW: maybe this should become a generic signal helper to delay signals
     by a certain time? *)
  (** The following is a mechanism to only set the search text in a delayed
      fashion. The [search_text_setter] signal contains an Lwt promise whose job
      is to set the search text after a delayed time. The [set_search_text]
      function cancels the current Lwt promise (which does nothing if it already
      resolved) and creates a new one in its place. *)
  let (text_setter, set_text_setter) = S.create Lwt.return_unit in
  let set_text text =
    (* try cancelling the current search text setter *)
    Lwt.cancel (S.value text_setter);
    (* prepare the new search text setter *)
    let new_text_setter =
      (* FIXME: here, we need to delay by something like 100ms but [Lwt_unix]
         does not seem to be the answer. *)
      Lwt.pmsleep 0.30;%lwt
      set_text_immediately text;
      Lwt.return_unit
    in
    (* register it in the signal *)
    set_text_setter new_text_setter;
    (* fire it asynchronously *)
    Lwt.async (fun () -> new_text_setter)
  in

  (** A signal that provides a {!state} view based on [text]. *)
  let state =
    S.bind_s' text StartTyping @@ fun text ->
    if String.length text < min_characters then
      (
        Lwt.return @@
        if text = "" then
          StartTyping
        else
          ContinueTyping
      )
    else
      Fun.flip Lwt.map (search text) @@ function
      | Error messages -> Errors messages
      | Ok [] -> NoResults
      | Ok results -> Results results
  in

  (** The HTML element of the bar; it is simply an input element with some
      customisation. *)
  let html =
    input
      ~a:(List.filter_map Fun.id [
          Some (a_input_type `Text);
          Some (a_placeholder placeholder);
          Some (
            a_oninput (fun event ->
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                  set_text (Js.to_string input##.value)
                );
                false
              )
          );
          (
            if autofocus then
              Some (a_autofocus ())
            else
              None
          );
          Option.map (fun f -> a_onfocus (fun _ -> f (); false)) on_focus;
          (
            Fun.flip Option.map on_enter @@ fun on_enter ->
            a_onkeyup (fun event ->
                if Js.Optdef.to_option event##.key = Some (Js.string "Enter") then
                  (
                    Js.Opt.iter event##.target @@ fun elt ->
                    Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                    on_enter (Js.to_string input##.value)
                  );
                true
              )
          );
        ]) ()
  in

  { state; set_text; html }

let quick_search ~placeholder ~search ~make_result ?on_enter ?autofocus () =


  (** A signal tracking whether the table is focused. *)
  let (table_visible, set_table_visible) = S.create false in

  let { state; html; _ } =
    make
      ~placeholder
      ~search
      ~on_focus:(fun () -> set_table_visible true)
      ?on_enter
      ?autofocus
      ()
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

    html;

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
          S.bind_s' state [] @@ function
          | StartTyping -> Lwt.return [emoji_row "👉" "Start typing to search."]
          | ContinueTyping -> Lwt.return [emoji_row "👉" (spf "Type at least %s characters." (Int.to_english_string min_characters))]
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
