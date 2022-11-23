module Log = (val Logs.src_log (Logs.Src.create "nes.unix.storage-cache") : Logs.LOG)

type hash = int

let compute_hash = Hashtbl.hash

let pp_hash fmt = Format.fprintf fmt "%x"
let hash_to_string = Format.sprintf "%x"

let hash_from_string repr =
  match int_of_string_opt @@ "0x" ^ repr with
  | None -> invalid_arg "NesStorageCache.hash_from_string"
  | Some hash -> hash

type ('a, 'b) t = (int, 'b) Hashtbl.t

(* FIXME: support lifetime; to be given on creation and cleaned up lazily on use
   and with an extra endpoint *)
let create () = Hashtbl.create 8

let pp_cache_identifier fmt cache =
  (* NOTE: For this one, we use [Hashtbl.hash]. It has big chances of collisions
     but it is fast and more than sufficient to give a debugging identifier. *)
  Format.fprintf fmt "%x" (Hashtbl.hash cache)

let add ~cache ~hash ~value =
  Log.debug (fun m -> m "Add hash %a in cache %a" pp_hash hash pp_cache_identifier cache);
  Hashtbl.add cache hash value

let use ~cache ~key thunk =
  let key = compute_hash key in
  Log.debug (fun m -> m "Looking for hash %a in cache %a" pp_hash key pp_cache_identifier cache);
  match Hashtbl.find_opt cache key with
  | Some value ->
    Log.debug (fun m -> m "Use cached hash %a in cache %a" pp_hash key pp_cache_identifier cache);
    value
  | None ->
    Log.debug (fun m -> m "Generate hash %a in cache %a" pp_hash key pp_cache_identifier cache);
    let value = thunk key in
    Log.debug (fun m -> m "Store hash %a in cache %a" pp_hash key pp_cache_identifier cache);
    Hashtbl.add cache key value;
    value

let remove ~cache ~key =
  let hash = compute_hash key in
  Log.debug (fun m -> m "Remove hash %a in cache %a" pp_hash hash pp_cache_identifier cache);
  Hashtbl.remove cache hash
