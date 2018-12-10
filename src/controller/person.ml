open Dancelor_common
open Dancelor_model
open QueryHelpers

let get query =
  let slug = query_string query "slug" in
  try
    Person.Database.get slug
    |> Person.view
    |> Person.view_to_jsonm
    |> (fun json -> Lwt.return (`O ["person", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this person does not exist"))
