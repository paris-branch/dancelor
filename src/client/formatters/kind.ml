open Nes
open Html

let full_string version tune =
  let base = Dancelor_common.Kind.Base.to_char @@ Model.Tune.kind tune in
  let bars = Model.Version.bars version in
  Lwt.return [txt (spf "%i %c" bars base)]
