open Nes
open Js_of_ocaml
open Html

type html =
  | Text of {input: 'a. ([> Html_types.input] as 'a) elt; input_dom: Dom_html.inputElement Js.t}
  | Textarea of {textarea: 'a. ([> Html_types.textarea] as 'a) elt; textarea_dom: Dom_html.textAreaElement Js.t}

type type_ = Text | Password | Textarea

module type Constants = sig
  type value
  val label : string
  val placeholder : string
  val type_ : type_
  val validator : string -> (value, string) result S.t
  val oninput : string -> unit
end

module Make (X : Constants) : Component.S with
  type value = X.value
  and type raw_value = string
= struct
  let label = X.label

  type value = X.value
  type raw_value = string

  let empty_value = ""

  type t = {
    raw_signal: string S.t;
    signal: (value, string) result S.t;
    set: string -> unit;
    html: html;
  }

  let raw_signal i = i.raw_signal

  let signal i = i.signal

  let focus i =
    match i.html with
    | Text {input_dom; _} ->
      input_dom##focus;
      let length = String.length (Js.to_string input_dom##.value) in
      input_dom##.selectionStart := length;
      input_dom##.selectionEnd := length
    | Textarea {textarea_dom; _} -> textarea_dom##focus

  let trigger = focus

  let clear i = i.set ""

  let make initial_value =
    let (raw_signal, set_immediately) = S.create initial_value in
    let set = S.delayed_setter 0.30 set_immediately in
    let signal = S.bind raw_signal X.validator in
    let html : html =
      match X.type_ with
      | Text | Password ->
        let input =
          input
            ()
            ~a: [
              a_input_type (match X.type_ with Text -> `Text | Password -> `Password | _ -> assert false);
              a_placeholder X.placeholder;
              R.a_value raw_signal;
              R.a_class (Component.case_errored ~no: ["form-control"; "is-valid"] ~yes: (const ["form-control"; "is-invalid"]) signal);
              a_oninput (fun event ->
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                  let input = Js.to_string input##.value in
                  set input;
                  X.oninput input
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
              a_placeholder X.placeholder;
              (* R.a_value state.raw_signal; FIXME: not possible in textarea but necessary for cleanup *)
              R.a_class (Component.case_errored ~no: ["form-control"; "is-valid"] ~yes: (const ["form-control"; "is-invalid"]) signal);
              a_oninput (fun event ->
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.textarea elt) @@ fun input ->
                  let input = Js.to_string input##.value in
                  set input;
                  X.oninput input
                );
                false
              );
            ]
        in
        Textarea {textarea; textarea_dom = To_dom.of_textarea textarea}
    in
      {raw_signal; signal; set; html}

  let inner_html i =
    match i.html with
    | Text {input; _} -> input
    | Textarea {textarea; _} -> textarea

  let html i =
    Component.render ~label ~signal: i.signal (inner_html i)
end

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
