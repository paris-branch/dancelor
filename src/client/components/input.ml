open Js_of_ocaml
open Dancelor_client_html

module Text = struct
  type 'a t = {
    validator : string -> ('a, string) Result.t;
    raw_signal : string S.t;
    set : string -> unit;
  }

  let make initial_value validator =
    let (raw_signal, set) = S.create initial_value in
    {validator; raw_signal; set}

  let raw_signal state = state.raw_signal

  let signal state = S.map state.validator state.raw_signal

  let clear state = state.set ""

  let render ~placeholder state =
    div ~a:[a_class ["input"]] [
      input ()
        ~a: [
          a_input_type `Text;
          a_placeholder placeholder;
          R.a_value state.raw_signal;
          R.a_class (
            Fun.flip S.map (signal state) @@ function
            | Ok _ -> []
            | Error _ -> ["invalid"]
          );
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
      R.div ~a:[a_class ["message-box"]] (
        Fun.flip S.map (signal state) @@ function
        | Ok _ -> []
        | Error msg -> [txt msg]
      );
    ]
end
