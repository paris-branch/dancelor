open Nes

let fold_to_hashtbl fold db k =
  let tbl = Hashtbl.create 8 in
  fold db (k (fun key value () -> Hashtbl.add tbl key value)) ();%lwt
  lwt tbl
