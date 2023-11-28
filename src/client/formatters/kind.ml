open Nes
open Dancelor_client_html
module M = Dancelor_client_model

let full_string version tune =
  let base = M.Kind.Base.to_char @@ M.Tune.kind tune in
  let bars = M.Version.bars version in
  Lwt.return [txt (spf "%i %c" bars base)]
