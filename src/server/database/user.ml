open Nes
include Tables.User

let get_from_username username =
  let all = get_all () in
  let this = Seq.filter ((=) username % Common.ModelBuilder.Core.User.username % Common.Entry.value) all in
  match this () with
  | Nil -> lwt_none
  | Cons (this, next) when next () = Nil -> lwt_some this
  | _ -> assert false
