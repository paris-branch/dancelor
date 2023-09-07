open Nes
open Dancelor_client_html

module M = Dancelor_client_model

let full_string version tune =
  let open Lwt in
  let%lwt base = M.Tune.kind tune >|= M.Kind.Base.to_char in
  let%lwt bars = M.Version.bars version in
  Lwt.return [text (spf "%i %c" bars base)]
