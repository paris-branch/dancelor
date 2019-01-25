open Dancelor_common
open Dancelor_model
open QueryHelpers
module Log = (val Log.create "dancelor.controller.program" : Logs.LOG)

let get query =
  let slug = query_string query "slug" in
  try
    Program.Database.get slug
    |> Program.to_jsonm
    |> (fun json -> Lwt.return (`O ["program", json]))
  with
    Not_found ->
    error "this set does not exist"

let get_all _query =
  Program.Database.get_all ()
  |> List.map Program.to_jsonm
  |> (fun json -> Lwt.return (`O ["programs", `A json]))
