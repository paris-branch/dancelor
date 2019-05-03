open Dancelor_server_model

let get person _ =
  person
  |> Dancelor_database.Person.get
  |> Person.to_jsonm
  |> (fun json -> Lwt.return (`O ["person", json]))
