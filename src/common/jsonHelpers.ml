module Log = (val Log.create "dancelor.common.json-helpers" : Logs.LOG)

let check_object = function
  | `O fields -> `O fields
  | _ -> failwith "Dancelor_common.JsonHelpers.check_object"

let add_field field value = function
  | `O fields when not (List.mem_assoc field fields) ->
     `O ((field, value) :: fields)
  | _ -> failwith "Dancelor_common.JsonHelpers.add_field"

let remove_field field = function
  | `O fields when List.mem_assoc field fields ->
     `O (List.remove_assoc field fields)
  | _ -> failwith "Dancelor_common.JsonHelpers.remove_field"

let find json path =
  Log.debug (fun m -> m "Finding path: %a" (ExtList.pp ~sep:" > " ExtString.pp) path);
  Ezjsonm.find json path

let find_opt json path =
  try Some (find json path)
  with Not_found -> None

let get_slug value =
  Slug.from_string (Ezjsonm.get_string value)
