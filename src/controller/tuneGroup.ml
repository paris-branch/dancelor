open Dancelor_common
open Dancelor_model

let get tune_group _ =
  tune_group
  |> TuneGroup.to_jsonm
  |> (fun json -> Lwt.return (`O ["tune-group", json]))
