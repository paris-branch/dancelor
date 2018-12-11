open ExtPervasives
open Option
module Log = (val Log.create "dancelor.common.serializer" : Logs.LOG)

let get_opt ~type_ path json =
  Log.debug (fun m -> m "Getting %a" (ExtList.pp ~sep:" > " ExtString.pp) path);
  (try Some (Ezjsonm.find json path)
   with Not_found -> None)
  >>= fun value ->
  Some (type_ value)

let get ~type_ path json =
  match get_opt ~type_ path json with
  | None -> raise Not_found
  | Some x -> x

let get_or ~type_ ~default path json =
  match get_opt ~type_ path json with
  | None -> default
  | Some x -> x

let int = Ezjsonm.get_int
let string = Ezjsonm.get_string
let slug = string ||> Slug.from_string
