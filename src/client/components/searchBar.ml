open Nes
open Js_of_ocaml
open Html

type 'result state =
  | StartTyping
  | ContinueTyping
  | Searching
  | NoResults
  | Results of 'result list
  | Errors of string

type 'result t = {
  text: string S.t; (* prefer [state] *)
  state: 'result state S.t;
  set_text: (string -> unit);
}

let make
    ~search
    ?(min_characters = 0)
    ~slice
    ?(on_number_of_entries = (const ()))
    ?(initial_input = "")
    ()
  =

  (** A signal containing the search text. *)
  let (text, set_immediately) = S.create initial_input in
  let set_text = S.delayed_setter 0.30 set_immediately in

  (** A signal that provides a {!state} view based on [text]. *)
  let state =
    S.bind slice @@ fun slice ->
    S.bind text @@ fun text ->
    if String.length text < min_characters then
      (
        S.const @@
          if text = "" then
            StartTyping
          else
            ContinueTyping
      )
    else
      (
        let search_signal = S.from' None (some <$> search slice text) in
        flip S.map search_signal @@ function
          | None -> Searching
          | Some Error messages ->
            Errors messages
          | Some Ok (_, []) ->
            NoResults
          | Some Ok (total, results) ->
            on_number_of_entries total; Results results
      )
  in
    {text; state; set_text}

(** FIXME: get rid of [on_focus] and [on_enter] that are probably not used
    anymore. *)

let render
    ~placeholder
    ?(autofocus = false)
    ?on_focus
    ?on_input
    ?on_enter
    search_bar
  =
  (* FIXME: This looks awfully like {!Input.Text.render}. We should probably
     build {!SearchBar} on top of that. *)
  let bar =
    input
      ~a: (
        List.filter_map Fun.id [
          Some (a_class ["form-control"]);
          Some (a_input_type `Text);
          Some (a_placeholder placeholder);
          Some (R.a_value search_bar.text);
          Some
            (
              a_oninput (fun event ->
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                  let input = Js.to_string input##.value in
                  search_bar.set_text input;
                  Option.value ~default: ignore on_input input;
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
        ]
      )
      ()
  in
  let bar' = To_dom.of_input bar in

  (* FIXME: This is a disgusting way to handle auto-focus. Instead, we should
     return some type that has a [focus] function and call it in client code. *)
  if autofocus then
    Lwt.async (fun () ->
      Js_of_ocaml_lwt.Lwt_js.sleep 1.;%lwt
      bar'##focus;
      lwt_unit
    );

  (* Because the following event prevents the default browser behaviour (in case
     of `on_enter`), it must happen on `keydown` and not on `keyup`. *)
  Utils.add_target_event_listener bar' Dom_html.Event.keydown (fun event _target ->
    match event##.keyCode with
    | 13 (* Enter *) ->
      (
        match on_enter with
        | None -> Js._true
        | Some on_enter ->
          (
            Js.Opt.iter event##.target @@ fun elt ->
            Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
            on_enter (Js.to_string input##.value)
          );
          Js._false
      );
    | 27 (* Esc *) -> (bar'##blur; Js._true)
    | _ -> Js._true
  );
  bar

let state search_bar = search_bar.state
let text search_bar = search_bar.text
let set_text search_bar text = search_bar.set_text text
let clear search_bar = search_bar.set_text ""
