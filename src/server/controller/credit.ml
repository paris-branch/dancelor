open Dancelor_server_model

let get credit _ =
  credit
  |> Dancelor_database.Credit.get
  |> Credit.to_jsonm
  |> (fun json -> Lwt.return (`O ["credit", json]))
