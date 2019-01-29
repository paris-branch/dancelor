open Dancelor_common
open Dancelor_model
open QueryHelpers

let get uri _ =
  let slug = List.assoc "slug" uri in
  try
    let tune = TuneGroup.Database.get slug in
    Lwt.return (`O [
      "tune-group", TuneGroup.to_jsonm tune;
    ])
  with
    Not_found -> error "this tune group does not exist"
