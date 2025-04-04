open Nes
open Js_of_ocaml
open Html

module Text = struct
  type 'a t = {
    validator: string -> ('a, string) Result.t;
    raw_signal: string S.t;
    set: string -> unit;
  }

  let make initial_value validator =
    let (raw_signal, set_immediately) = S.create initial_value in
    let set = S.delayed_setter 0.30 set_immediately in
    {validator; raw_signal; set}

  let raw_signal state = state.raw_signal

  let signal state = S.map state.validator state.raw_signal

  let case_errored ~no ~yes state =
    Fun.flip S.map (signal state) @@ function
    | Error msg -> yes msg
    | _ -> no

  let clear state = state.set ""

  let render ?label: lbl ?(placeholder = "") state =
    div
      ~a: [a_class ["mb-2"]]
      [
        label ~a: [a_class ["form-label"]] (Option.to_list (Option.map txt lbl));
        input
          ()
          ~a: [
            a_input_type `Text;
            a_placeholder placeholder;
            R.a_value state.raw_signal;
            R.a_class (case_errored ~no: ["form-control"; "is-valid"] ~yes: (Fun.const ["form-control"; "is-invalid"]) state);
            a_oninput (fun event ->
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.input elt) @@ fun input ->
                  let input = Js.to_string input##.value in
                  state.set input
                );
                false
              );
          ];
        R.div
          ~a: [R.a_class (case_errored ~no: ["valid-feedback"] ~yes: (Fun.const ["invalid-feedback"]) state)]
          (
            case_errored ~no: [txt "Looks good!"] ~yes: (List.singleton % txt) state
          );
      ]

  let render_as_textarea ?label: lbl ?(placeholder = "") state =
    div
      ~a: [a_class ["mb-2"]]
      [
        label ~a: [a_class ["form-label"]] (Option.to_list (Option.map txt lbl));
        textarea
          (R.txt state.raw_signal)
          ~a: [
            a_rows 15;
            a_placeholder placeholder;
            R.a_class (case_errored ~no: ["form-control"; "is-valid"] ~yes: (Fun.const ["form-control"; "is-invalid"]) state);
            a_oninput (fun event ->
                (
                  Js.Opt.iter event##.target @@ fun elt ->
                  Js.Opt.iter (Dom_html.CoerceTo.textarea elt) @@ fun input ->
                  let input = Js.to_string input##.value in
                  state.set input
                );
                false
              );
          ];
        R.div
          ~a: [R.a_class (case_errored ~no: ["valid-feedback"] ~yes: (Fun.const ["invalid-feedback"]) state)]
          (
            case_errored ~no: [txt "Looks good!"] ~yes: (List.singleton % txt) state
          );
      ]
end
