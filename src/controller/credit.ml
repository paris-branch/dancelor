open Dancelor_common
open Dancelor_model
open QueryHelpers

let get query =
  let slug = query_string query "slug" in
  try
    Credit.Database.get slug
    |> Credit.to_jsonm
    |> (fun json -> Lwt.return (`O ["credit", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this credit does not exist"))
