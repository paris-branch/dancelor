open ExtPervasives
module Log = (val Log.create "dancelor.common.serializer" : Logs.LOG)

let get_opt ~type_ path json =
  Log.debug (fun m -> m "Getting %a" (ExtList.pp ~sep:" > " ExtString.pp) path);
  Json.get_opt ~k:type_ path json

let get ~type_ path json =
  match get_opt ~type_ path json with
  | None -> raise Not_found
  | Some x -> x

let get_or ~type_ ~default path json =
  match get_opt ~type_ path json with
  | None -> default
  | Some x -> x

let int = Json.int
let string = Json.string
let slug = Json.string ||> Slug.from_string
