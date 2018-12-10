open Dancelor_common
open Dancelor_model
open QueryHelpers

let get query =
  let slug = query_string query "slug" in
  try
    Set.Database.get slug
    |> Set.to_jsonm
    |> (fun json -> Lwt.return (`O ["set", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this set does not exist"))

let get_all _query =
  Set.Database.get_all ()
  |> List.map Set.to_jsonm
  |> (fun json -> Lwt.return (`O ["sets", `A json]))

let compose query =
  let open Option in
  let name = `String (value ~default:"" (query_string_opt query "name")) in
  let kind = `String (value ~default:"8x32R" (query_string_opt query "kind")) in
  let tune_jsons, error_jsons =
    match query_strings_opt query "tunes[]" with
    | Some tunes ->
       List.map_partition
         (fun tune ->
           try
             Tune.Database.get tune
             |> Tune.to_jsonm
             |> (fun json -> List.A json)
           with
             Not_found -> List.B (`O ["message", `String ("tune \"" ^ tune ^ "\" does not exist")]))
         tunes
    | None ->
       [], []
  in
  let set_json = `O [ "name", name ; "kind", kind ; "tunes", `A tune_jsons ; "errors", `A error_jsons ] in
  let all_tune_jsons =
    Tune.Database.get_all ()
    |> List.map (fun (_, tune, version) ->
           `O [
               "tune", Tune.to_jsonm tune;
               "versions", Tune.Version.to_jsonm version
         ])
  in
  Lwt.return (`O [ "set", set_json ; "all_tunes", `A all_tune_jsons ])
