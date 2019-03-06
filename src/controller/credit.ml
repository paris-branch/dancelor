open Dancelor_common
open Dancelor_model

let get credit _ =
  credit
  |> Dancelor_database.Credit.get
  |> Credit.to_jsonm
  |> (fun json -> Lwt.return (`O ["credit", json]))
