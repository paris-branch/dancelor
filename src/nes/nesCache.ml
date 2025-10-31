let now () = int_of_float @@ Unix.gettimeofday ()

type 'v entry = {
  value: 'v;
  created_at: int;
}

let entry_age ~now entry = now - entry.created_at

type ('k, 'v) t = {
  entries: ('k, 'v entry) Hashtbl.t;
  entry_lifetime: int;
  cleanup_every: int;
  mutable last_cleanup: int;
}

let create ?(lifetime = max_int) () = {
  entries = Hashtbl.create 8;
  entry_lifetime = lifetime;
  cleanup_every = lifetime;
  last_cleanup = now ();
}

let cleanup ~cache =
  let now = now () in
  Hashtbl.filter_map_inplace
    (fun _ entry ->
      if entry_age ~now entry > cache.entry_lifetime then None
      else Some entry
    )
    cache.entries;
  cache.last_cleanup <- now

let cleanup_if_due ~cache ~now =
  if now - cache.last_cleanup > cache.cleanup_every then
    cleanup ~cache

let use ~cache ~key thunk =
  let now = now () in
  cleanup_if_due ~cache ~now;
  match Hashtbl.find_opt cache.entries key with
  | Some entry when entry_age ~now entry <= cache.entry_lifetime ->
    entry.value
  | _ ->
    let value = thunk () in
    Hashtbl.replace cache.entries key {value; created_at = now};
    value
