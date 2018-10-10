open Common

let get query =
  let slug = query_string query "slug" in
  try
    Credit.Database.get slug
    |> Credit.view
    |> Credit.view_to_jsonm
    |> (fun json -> success ["credit", json])
  with
    Not_found -> error `Not_found "this credit does not exist"
