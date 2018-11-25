open Dancelor_common
open Dancelor_model
open LwtResultMonad
open QueryHelpers

let get query =
  Lwt.return (query_string query "slug") >>= fun slug ->
  try
    Person.Database.get slug
    |> Person.view
    |> Person.view_to_jsonm
    |> (fun json -> Lwt.return (Ok (`O ["person", json])))
  with
    Not_found ->
    Lwt.return (Error "this person does not exist")
