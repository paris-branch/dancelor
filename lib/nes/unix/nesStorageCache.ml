module Log = (val Logs.src_log (Logs.Src.create "nes.unix.storage-cache") : Logs.LOG)

type hash = int

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

let add ~cache ~hash ~value =
  Log.debug (fun m -> m "Add hash %a in cache %x" pp_hash hash (Hashtbl.hash cache));
  Hashtbl.add cache hash value

let use ~cache ~key thunk =
  let key = Hashtbl.hash key in
  Log.debug (fun m -> m "Looking for hash %a in cache %x" pp_hash key (Hashtbl.hash cache));
  match Hashtbl.find_opt cache key with
  | Some value ->
    Log.debug (fun m -> m "Use cached hash %a in cache %x" pp_hash key (Hashtbl.hash cache));
    value
  | None ->
    Log.debug (fun m -> m "Generate hash %a in cache %x" pp_hash key (Hashtbl.hash cache));
    let value = thunk key in
    Log.debug (fun m -> m "Store hash %a in cache %x" pp_hash key (Hashtbl.hash cache));
    Hashtbl.add cache key value;
    value

let remove ~cache ~key =
  let hash = Hashtbl.hash key in
  Log.debug (fun m -> m "Remove hash %a in cache %x" pp_hash hash (Hashtbl.hash cache));
  Hashtbl.remove cache hash
