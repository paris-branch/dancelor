open Nes
open Js_of_ocaml
open Html

type html =
  | Text of Html_types.input elt * Dom_html.inputElement Js.t
  | Textarea of Html_types.textarea elt * Dom_html.textAreaElement Js.t

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
  | Text (_, input) ->
    input##focus;
    let length = String.length (Js.to_string input##.value) in
    input##.selectionStart := length;
    input##.selectionEnd := length
  | Textarea (_, textarea) -> textarea##focus

let clear state = state.set ""

let case_errored ~no ~yes signal =
  flip S.map signal @@ function
    | Error msg -> yes msg
    | _ -> no

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
      let html =
        input
          ()
          ~a: [
            a_input_type (match type_ with Text -> `Text | Password -> `Password | _ -> assert false);
            a_placeholder placeholder;
            R.a_value raw_signal;
            R.a_class (case_errored ~no: ["form-control"; "is-valid"] ~yes: (const ["form-control"; "is-invalid"]) signal);
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
      Text (html, To_dom.of_input html)
    | Textarea ->
      let textarea =
        textarea
          (R.txt raw_signal)
          ~a: [
            a_rows 15;
            a_placeholder placeholder;
            (* R.a_value state.raw_signal; FIXME: not possible in textarea but necessary for cleanup *)
            R.a_class (case_errored ~no: ["form-control"; "is-valid"] ~yes: (const ["form-control"; "is-invalid"]) signal);
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
      Textarea (textarea, To_dom.of_textarea textarea)
  in
    {label; raw_signal; signal; set; html}

let make ?label ?placeholder ?oninput ~type_ ~initial_value ~validator () =
  make' ?label ?placeholder ?oninput ~type_ ~initial_value ~validator: (S.const % validator) ()

let html textInput =
  div
    ~a: [a_class ["mb-2"]]
    [
      label ~a: [a_class ["form-label"]] (Option.to_list (Option.map txt textInput.label));
      (
        match textInput.html with
        | Text (input, _) -> ((tot % toelt) input: [> Html_types.input] elt)
        | Textarea (textarea, _) -> ((tot % toelt) textarea: [> Html_types.textarea] elt)
      );
      R.div
        ~a: [R.a_class (case_errored ~no: ["valid-feedback"] ~yes: (const ["invalid-feedback"]) textInput.signal)]
        (
          case_errored ~no: [txt " "] ~yes: (List.singleton % txt) textInput.signal
        );
    ]

let inactive ?label: lbl value =
  div
    ~a: [a_class ["mb-2"]]
    [
      label ~a: [a_class ["form-label"]] (Option.to_list (Option.map txt lbl));
      input
        ()
        ~a: [
          a_input_type `Text;
          a_value value;
          a_class ["form-control"];
          a_disabled ();
        ];
      div ~a: [a_class ["valid-feedback"; "d-block"]] [txt " "];
    ]
