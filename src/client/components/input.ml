open Nes
open Js_of_ocaml
open Html

type html =
  | Text of {input: 'a. ([> Html_types.input] as 'a) elt; input_dom: Dom_html.inputElement Js.t}
  | Textarea of {textarea: 'a. ([> Html_types.textarea] as 'a) elt; textarea_dom: Dom_html.textAreaElement Js.t}

type type_ = Text | Password | Textarea

type 'a t = {
  label: string option;
  raw_signal: string S.t;
  signal: ('a, string) result S.t;
  set: string -> unit;
  html: html;
}

let raw_signal state = state.raw_signal

let signal state = state.signal
let value state = S.value @@ signal state

let focus state =
  match state.html with
  | Text {input_dom; _} ->
    input_dom##focus;
    let length = String.length (Js.to_string input_dom##.value) in
    input_dom##.selectionStart := length;
    input_dom##.selectionEnd := length
  | Textarea {textarea_dom; _} -> textarea_dom##focus

let trigger = focus

let clear state = state.set ""

let make'
    ?label
    ?(placeholder = "")
    ?(oninput = ignore)
    ~type_
    ~initial_value
    ~validator
    ()
  =
  let (raw_signal, set_immediately) = S.create initial_value in
  let set = S.delayed_setter 0.30 set_immediately in
  let signal = S.bind raw_signal validator in
  let html : html =
    match type_ with
    | Text | Password ->
      let input =
        input
          ()
          ~a: [
            a_input_type (match type_ with Text -> `Text | Password -> `Password | _ -> assert false);
            a_placeholder placeholder;
            R.a_value raw_signal;
            R.a_class (Component.case_errored ~no: ["form-control"; "is-valid"] ~yes: (const ["form-control"; "is-invalid"]) signal);
            a_oninput (fun event ->
              (
                Js.Opt.iter event##.target @@ fun elt ->
                Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                let input = Js.to_string input##.value in
                set input;
                oninput input
              );
              false
            );
          ]
      in
      Text {input; input_dom = To_dom.of_input input}
    | Textarea ->
      let textarea =
        textarea
          (R.txt raw_signal)
          ~a: [
            a_rows 15;
            a_placeholder placeholder;
            (* R.a_value state.raw_signal; FIXME: not possible in textarea but necessary for cleanup *)
            R.a_class (Component.case_errored ~no: ["form-control"; "is-valid"] ~yes: (const ["form-control"; "is-invalid"]) signal);
            a_oninput (fun event ->
              (
                Js.Opt.iter event##.target @@ fun elt ->
                Js.Opt.iter (Dom_html.CoerceTo.textarea elt) @@ fun input ->
                let input = Js.to_string input##.value in
                set input;
                oninput input
              );
              false
            );
          ]
      in
      Textarea {textarea; textarea_dom = To_dom.of_textarea textarea}
  in
    {label; raw_signal; signal; set; html}

let make ?label ?placeholder ?oninput ~type_ ~initial_value ~validator () =
  make' ?label ?placeholder ?oninput ~type_ ~initial_value ~validator: (S.const % validator) ()

let inner_html textInput =
  match textInput.html with
  | Text {input; _} -> input
  | Textarea {textarea; _} -> textarea

let html textInput =
  Component.render ?label: textInput.label ~signal: textInput.signal (inner_html textInput)

let inactive ?label value =
  Component.render ?label ~signal: (S.const (Ok ())) @@
    input
      ()
      ~a: [
        a_input_type `Text;
        a_value value;
        a_class ["form-control"];
        a_disabled ();
      ]
