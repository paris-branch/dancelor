open Dancelor_common
open Dancelor_model
open QueryHelpers

let get query _body =
  let slug = query_string query "slug" in
  try
    Set.Database.get slug
    |> Set.view
    |> Set.view_to_jsonm
    |> (fun json -> Lwt.return (`O ["set", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this set does not exist"))
