open Dancelor_common
open Dancelor_model
open LwtResultMonad
open QueryHelpers

let get query =
  Lwt.return (query_string query "slug") >>= fun slug ->
  try
    Credit.Database.get slug
    |> Credit.view
    |> Credit.view_to_jsonm
    |> (fun json -> Lwt.return (Ok (`O ["credit", json])))
  with
    Not_found ->
    Lwt.return (Error "this credit does not exist")
