open Nes

let fold_to_get_single fold k =
  let t = Hashtbl.create 8 in
  fold (k (fun key value () -> assert (not @@ Hashtbl.mem t key); Hashtbl.add t key value)) ();%lwt
  lwt (Hashtbl.find_opt t)

let fold_to_get_list fold k =
  let t = Hashtbl.create 8 in
  fold (k (fun key value () -> Hashtbl.add t key value)) ();%lwt
  lwt (List.rev % Hashtbl.find_all t)

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
