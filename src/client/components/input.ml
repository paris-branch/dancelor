open Js_of_ocaml
open Dancelor_client_html

module Text = struct
  type 'a t = {
    signal : ('a, string) Result.t S.t;
    set : string -> unit;
  }

  let make f =
    let (signal, set) = S.create "" in
    let signal = S.map f signal in
    {signal; set}

  let render ~placeholder state =
    div ~a:[a_class ["input"]] [
      input ()
        ~a: [
          a_input_type `Text;
          a_placeholder placeholder;
          R.a_class (
            Fun.flip S.map state.signal @@ function
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
        Fun.flip S.map state.signal @@ function
        | Ok _ -> []
        | Error msg -> [txt msg]
      );
    ]
end
