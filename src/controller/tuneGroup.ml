open Dancelor_common
open Dancelor_model
open QueryHelpers
module Log = (val Log.create "dancelor.controller.tune" : Logs.LOG)

let get query =
  try
    let slug = query_string query "slug" in
    let tune = Tune.Database.get slug in
    Lwt.return (`O [
      "tune", Tune.to_jsonm tune;
    ])
  with
    Not_found -> error "this tune group does not exist"
