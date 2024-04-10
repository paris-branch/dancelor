open Nes
open Js_of_ocaml
open Dancelor_client_html
open Dancelor_client_model

(** Generic row showing an emoji on the left and a message on the right. *)
let fa_row icon message =
  tr [
    td ~a:[a_colspan 0] [
      i ~a:[a_class ["material-symbols-outlined"]] [txt icon];
      txt " ";
      txt message;
    ];
  ]

type 'result state =
  | StartTyping
  | ContinueTyping
  | NoResults
  | Results of 'result list
  | Errors of string

type 'result t = {
  text : string S.t; (* prefer [state] *)
  state : 'result state S.t;
  set_text : (string -> unit);
}

let make
    ~search
    ?(min_characters=0)
    ~slice
    ?(on_number_of_entries=(Fun.const ()))
    ?(initial_input = "")
    ()
  =
  (** A signal containing the search text. *)
  let (text, set_text_immediately) = S.create initial_input in

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
    S.bind slice @@ fun slice ->
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
      Fun.flip Lwt.map (search slice text) @@ function
      | Error messages ->
        Errors messages
      | Ok (_, []) ->
        NoResults
      | Ok (total, results) ->
        on_number_of_entries total; Results results
  in

  { text; state; set_text }

let render ~placeholder ?(autofocus=false) ?on_focus ?on_input ?on_enter search_bar =
  input
    ~a:(List.filter_map Fun.id [
        Some (a_input_type `Text);
        Some (a_placeholder placeholder);
        Some (R.a_value search_bar.text);
        Some (
          a_oninput (fun event ->
              (
                Js.Opt.iter event##.target @@ fun elt ->
                Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                let input = Js.to_string input##.value in
                search_bar.set_text input;
                Option.value ~default:ignore on_input input;
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
              (* NOTE: Enter is decimal key code 13. One could also use
                 event##.key and check that it is ["Enter"] but it is somehow
                 sometimes not set (eg. in Selenium or on mobile phones). *)
              if event##.keyCode = 13 then
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                  on_enter (Js.to_string input##.value)
                );
              true
            )
        );
      ]) ()

let state search_bar = search_bar.state
let text search_bar = search_bar.text
let set_text search_bar text = search_bar.set_text text

module Quick = struct
  (* FIXME: We should push the selection into the search bar, instead of
     providing a [make_result] that gives a clickable row. That way, we could
     improve the search bar to work with keys and not just clicks. *)

  let make_and_render ?(number_of_results=10) ~placeholder ~search ~make_result ?on_enter ?autofocus () =
    let min_characters = 3 in
    let slice = S.const @@ Slice.make ~start:0 ~end_excl:number_of_results () in

    (** A signal tracking whether the table is focused. *)
    let (table_visible, set_table_visible) = S.create false in

    let search_bar = make ~search ~min_characters ~slice () in

    div ~a:[a_class ["search-bar"]] [
      div ~a:[
        R.a_class (
          Fun.flip S.map table_visible @@ function
          | false -> []
          | true -> ["visible"]
        );
        a_onclick (fun _ -> set_table_visible false; false);
      ] [];

      render ~placeholder ~on_focus:(fun () -> set_table_visible true) ?on_enter ?autofocus search_bar;

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
            S.bind_s' (state search_bar) [] @@ function
            | StartTyping -> Lwt.return [fa_row "keyboard" "Start typing to search."]
            | ContinueTyping -> Lwt.return [fa_row "keyboard" (spf "Type at least %s characters." (Int.to_english_string min_characters))]
            | NoResults -> Lwt.return [fa_row "warning" "Your search returned no results."]
            | Errors error -> Lwt.return [fa_row "error" error]
            | Results results ->
              let%lwt results = Lwt_list.map_p make_result results in
              Lwt.return @@
              if on_enter = None then
                results
              else
                results @ [fa_row "info" "Press enter for more results."]
          );
        ]
    ]
end
