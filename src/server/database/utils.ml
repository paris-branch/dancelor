open Nes

type ('k, 'v) tbl = Tbl of ('k, 'v) Hashtbl.t

let fold_to_tbl fold db k =
  let t = Hashtbl.create 8 in
  fold db (k (fun key value () -> Hashtbl.add t key value)) ();%lwt
  lwt @@ Tbl t

let tbl_get (Tbl t) k =
  List.rev @@ Hashtbl.find_all t k

let fold_to_get fold db k =
  let%lwt tbl = fold_to_tbl fold db k in
  lwt @@ tbl_get tbl

let list_option_map_to_sql f = function
  | None -> `None
  | Some x -> `Some (List.map f x)
