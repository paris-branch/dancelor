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

let compose query _body =
  let name = `String (value ~default:"" (query_string_opt query "name")) in
  let kind = `String (value ~default:"8x32R" (query_string_opt query "kind")) in
  let tunes, errors =
    match query_strings_opt query "tunes[]" with
    | Some tunes ->
       List.map_partition
         (fun tune ->
           try
             Tune.Database.get tune
             |> Tune.view
             |> Tune.view_to_jsonm
             |> (fun json -> List.A json)
           with
             Not_found -> List.B (`String ("tune \"" ^ tune ^ "\" does not exist")))
         tunes
    | None ->
       [], []
  in
  let json = `O [ "set", `O [ "name", name ; "kind", kind ;
                              "tunes", `A tunes ; "errors", `A errors ] ]
  in
  Lwt.return json
