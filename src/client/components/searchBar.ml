open Nes
open Js_of_ocaml
open Js_of_ocaml_lwt
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
  bar_html: 'a. ([> Html_types.input] as 'a) elt;
  bar_dom: Dom_html.inputElement Js.t;
}

let make
    ~search
    ?(min_characters = 0)
    ~slice
    ?(on_number_of_entries = (const ()))
    ?(initial_input = "")
    ~placeholder
    ?on_focus
    ?on_input
    ?on_enter
    ()
  =

  (** A signal containing the search text. *)
  let (text, set_text) = S.create initial_input in

  (** Typing a character cancels the previous search and replaces it by a new
      one. This is exactly {!NesLwt.replaceable}. *)
  let replace_promise = Lwt.replaceable () in

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
        let delayed_search_promise = replace_promise (Lwt_js.sleep 1.;%lwt search slice text) in
        let search_signal = S.from' None (some <$> delayed_search_promise) in
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

  (* FIXME: This looks awfully like {!Input.render}. We should probably
     build {!SearchBar} on top of that. *)
  let bar_html =
    input
      ~a: (
        List.filter_map Fun.id [
          Some (a_class ["form-control"]);
          Some (a_input_type `Text);
          Some (a_placeholder placeholder);
          Some (R.a_value text);
          Some
            (
              a_oninput (fun event ->
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                  let input = Js.to_string input##.value in
                  set_text input;
                  Option.value ~default: ignore on_input input;
                );
                false
              )
            );
          Option.map (fun f -> a_onfocus (fun _ -> f (); false)) on_focus;
        ]
      )
      ()
  in
  let bar_dom = To_dom.of_input bar_html in

  (* Because the following event prevents the default browser behaviour (in case
     of `on_enter`), it must happen on `keydown` and not on `keyup`. *)
  Utils.add_target_event_listener bar_dom Dom_html.Event.keydown (fun event _target ->
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
    | 27 (* Esc *) -> (bar_dom##blur; Js._true)
    | _ -> Js._true
  );
  {text; state; set_text; bar_html; bar_dom}

let state search_bar = search_bar.state
let text search_bar = search_bar.text
let set_text search_bar text = search_bar.set_text text
let clear search_bar = search_bar.set_text ""
let html search_bar = search_bar.bar_html

let focus search_bar =
  search_bar.bar_dom##focus;
  let length = String.length (Js.to_string search_bar.bar_dom##.value) in
  search_bar.bar_dom##.selectionStart := length;
  search_bar.bar_dom##.selectionEnd := length
