open Dancelor_common
open Dancelor_model

let get uri _ =
  let slug = List.assoc "slug" uri in
  try
    Credit.Database.get slug
    |> Credit.to_jsonm
    |> (fun json -> Lwt.return (`O ["credit", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this credit does not exist"))
