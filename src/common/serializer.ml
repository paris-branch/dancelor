open ExtPervasives
module Log = (val Log.create "dancelor.common.serializer" : Logs.LOG)

let get ~type_ path json =
  Log.debug (fun m -> m "Getting %a" (ExtList.pp ~sep:" > " ExtString.pp) path);
  type_ (Ezjsonm.find json path)

let get_opt ~type_ path json =
  try Some (get ~type_ path json)
  with Not_found -> None

let get_or ~type_ ~default path json =
  try get ~type_ path json
  with Not_found -> default

let int = Ezjsonm.get_int
let string = Ezjsonm.get_string
let slug = string ||> Slug.from_string
