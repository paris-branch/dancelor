open Dancelor_common
open Dancelor_model
open QueryHelpers

let get query _body =
  let slug = query_string query "slug" in
  try
    Credit.Database.get slug
    |> Credit.view
    |> Credit.view_to_jsonm
    |> (fun json -> Lwt.return (`O ["credit", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this credit does not exist"))
