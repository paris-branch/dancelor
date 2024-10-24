open Nes
open Js_of_ocaml
open Dancelor_client_html

module Text = struct
  type 'a t = {
    has_interacted: bool S.t;
    set_interacted: unit -> unit;
    validator: string -> ('a, string) Result.t;
    raw_signal: string S.t;
    set: string -> unit;
  }

  let make ?(has_interacted = S.const false) initial_value validator =
    let (has_interacted_locally, set_interacted) = S.create false in
    let has_interacted = S.l2 (||) has_interacted has_interacted_locally in
    let set_interacted () =
      set_interacted true;
      S.stop ~strong: true has_interacted
    in
    let (raw_signal, set_immediately) = S.create initial_value in
    let set = S.delayed_setter 0.30 set_immediately in
    {has_interacted; set_interacted; validator; raw_signal; set}

  let raw_signal state = state.raw_signal

  let signal state = S.map state.validator state.raw_signal

  let has_interacted state = state.has_interacted

  let case_errored ~no ~yes state =
    S.bind (has_interacted state) @@ fun has_interacted ->
    Fun.flip S.map (signal state) @@ function
      | Error msg when has_interacted -> yes msg
      | _ -> no

  let clear state = state.set ""

  let render ?label: lbl ?(placeholder = "") state =
    div
      ~a: [a_class ["form-element"]]
      [
        label (Option.to_list (Option.map txt lbl));
        input
          ()
          ~a: [
            a_input_type `Text;
            a_placeholder placeholder;
            R.a_value state.raw_signal;
            R.a_class (case_errored ~no: [] ~yes: (Fun.const ["invalid"]) state);
            a_oninput (fun event ->
              (
                Js.Opt.iter event##.target @@ fun elt ->
                Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                let input = Js.to_string input##.value in
                state.set input
              );
              false
            );
            a_onfocus (fun _ -> state.set_interacted (); false);
          ];
        R.div
          ~a: [a_class ["message-box"]]
          (
            case_errored ~no: [] ~yes: (List.singleton % txt) state
          );
      ]

  let render_as_textarea ?label: lbl ?(placeholder = "") state =
    div
      ~a: [a_class ["form-element"]]
      [
        label (Option.to_list (Option.map txt lbl));
        textarea
          (R.txt state.raw_signal)
          ~a: [
            a_rows 15;
            a_placeholder placeholder;
            R.a_class (case_errored ~no: [] ~yes: (Fun.const ["invalid"]) state);
            a_oninput (fun event ->
              (
                Js.Opt.iter event##.target @@ fun elt ->
                Js.Opt.iter (Dom_html.CoerceTo.textarea elt) @@ fun input ->
                let input = Js.to_string input##.value in
                state.set input
              );
              false
            );
            a_onfocus (fun _ -> state.set_interacted (); false);
          ];
        R.div
          ~a: [a_class ["message-box"]]
          (
            case_errored ~no: [] ~yes: (List.singleton % txt) state
          );
      ]
end
