open Nes
open Common

open Html

let full_string version tune =
  let base = Kind.Base.to_char @@ Model.Tune.kind' tune in
  let bars = Model.Version.bars' version in
  Lwt.return [txt (spf "%i %c" bars base)]
