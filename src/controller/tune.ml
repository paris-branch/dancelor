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
    Not_found -> error "this tune does not exist"

let get_all query =
  let tune_jsons =
    Tune.Database.get_all
      ?name:(query_string_opt query "name")
      ?author:(query_string_opt query "author")
      ?kind:(
        let kind = query_string_or query "kind" "" in
        if kind = "" then
          None
        else
          try
            Some (Kind.base_of_string kind)
          with
            Failure _ -> error "kind must be 'j', 'p', 'r', 's' or 'w'"
      )
      ?keys:(
        let open Option in
        query_string_opt query "keys" >>= fun keys ->
        let keys = String.split_on_char ',' keys in
        Some (List.map Music.key_of_string keys)
      )
      ?mode:(
        let open Option in
        query_string_opt query "mode" >>= function
        | "major" -> Some Music.Major
        | "minor" -> Some Music.Minor
        | _ -> error ("mode must be major or minor")
      )
      ()
    |> List.map (fun (score, tune, version) ->
           Tune.tune_version_to_json (tune, version)
           |> Json.add_field "score" (`Float (floor (100. *. score)))
           |> Json.to_value)
    |> (fun jsons -> `A jsons)
  in
  Lwt.return (
      `O [
          "tunes", tune_jsons;
          "query",
          `O [
              "name", `String (query_string_or query "name" "");
              "author", `String (query_string_or query "author" "");
            ]
        ]
    )
