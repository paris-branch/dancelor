open Dancelor_common
open Dancelor_model

let get uri _ =
  let slug = List.assoc "slug" uri in
  try
    Person.Database.get slug
    |> Person.to_jsonm
    |> (fun json -> Lwt.return (`O ["person", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this person does not exist"))
