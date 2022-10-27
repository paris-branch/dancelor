type ('a, 'b) t = (int, 'b) Hashtbl.t

(* FIXME: support lifetime; to be given on creation and cleaned up lazily on use
   and with an extra endpoint *)
let create () = Hashtbl.create 8

let add ~cache ~hash ~value =
  Hashtbl.add cache hash value

let use ~cache ~key thunk =
  let key = Hashtbl.hash key in
  match Hashtbl.find_opt cache key with
  | Some value ->
    value
  | None ->
    let value = thunk () in
    Hashtbl.add cache key value;
    value

let remove ~cache ~key =
  Hashtbl.remove cache (Hashtbl.hash key)
