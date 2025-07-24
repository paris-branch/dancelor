open Nes
include Tables.User

let get id = get id
let get_all () = get_all ()

let get_from_username username =
  let%lwt all = get_all () in
  let this = List.filter ((=) username % Common.ModelBuilder.Core.User.username % Common.Entry.value) all in
  match this with
  | [this] -> lwt_some this
  | [] -> lwt_none
  | _ -> assert false
