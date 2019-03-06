open Dancelor_common
open Dancelor_model

let get person _ =
  person
  |> Dancelor_database.Person.get
  |> Person.to_jsonm
  |> (fun json -> Lwt.return (`O ["person", json]))
