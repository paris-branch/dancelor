type ('a, 'b) t = ('a, 'b) Hashtbl.t

(* FIXME: support lifetime; to be given on creation and cleaned up lazily on use
   and with an extra endpoint *)
let create () = Hashtbl.create 8

let use ~cache ~key thunk =
  match Hashtbl.find_opt cache key with
  | Some value ->
    value
  | None ->
    let value = thunk () in
    Hashtbl.add cache key value;
    value

let remove ~cache ~key =
  Hashtbl.remove cache key
