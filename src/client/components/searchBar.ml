open Nes
open Js_of_ocaml
open Dancelor_client_html

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
  (* FIXME: This looks awfully like {!Input.Text.render}. We should probably
     build {!SearchBar} on top of that. *)
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
let clear search_bar = search_bar.set_text ""
