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

type 'a sql_option = [`None | `Some of 'a]

let option_to_sql : 'a option -> 'a sql_option = function
  | None -> `None
  | Some x -> `Some x

let sql_to_option : 'a sql_option -> 'a option = function
  | `None -> None
  | `Some x -> Some x

type 'a all_or_one_of = [`All | `One_of of 'a list]

let map_all_or_one_of f = function
  | `All -> `All
  | `One_of xs -> `One_of (List.map f xs)
