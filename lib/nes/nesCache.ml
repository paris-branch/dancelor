type ('a, 'b) t = ('a, 'b) Hashtbl.t

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
