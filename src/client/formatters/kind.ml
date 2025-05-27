open Nes
open Common

open Html

let full_string version tune =
  with_span_placeholder @@
    let base = Kind.Base.to_char @@ Model.Tune.kind' tune in
    let bars = Model.Version.bars' version in
    lwt [txt (spf "%i %c" bars base)]
