open Dancelor_common
open Dancelor_model
open LwtResultMonad
open QueryHelpers

let get query =
  Lwt.return (query_string query "slug") >>= fun slug ->
  try
    Set.Database.get slug
    |> Set.view
    |> Set.view_to_jsonm
    |> (fun json -> Lwt.return (Ok (`O ["set", json])))
  with
    Not_found ->
    Lwt.return (Error "this set does not exist")
