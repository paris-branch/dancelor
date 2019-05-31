open Js_of_ocaml
open Dancelor_client_model

module Html = Dom_html

let js = Js.string


module Kind = struct

  let full_string tune group =
    let open Lwt in
    let%lwt base = TuneGroup.kind group >|= Kind.base_to_char in
    let%lwt bars = Tune.bars tune in
    Lwt.return (Printf.sprintf "%i %c" bars base)

end

module Credit = struct

  let line = function
    | None -> Lwt.return ""
    | Some c -> Credit.line c

end
