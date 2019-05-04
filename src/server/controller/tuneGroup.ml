open Dancelor_server_model

let get tune_group _ =
  tune_group
  |> Dancelor_database.TuneGroup.get
  |> TuneGroup.to_yojson
  |> Lwt.return
