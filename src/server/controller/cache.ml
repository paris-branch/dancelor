module Log = (val Dancelor_server_logs.create "controller.cache" : Logs.LOG)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

let create () =
  Log.debug (fun m -> m "Creating new cache");
  Hashtbl.create 8

let use cache key thunk =
  match Hashtbl.find_opt cache key with
  | Some value ->
     Log.debug (fun m -> m "Found in cache");
     value
  | None ->
     Log.debug (fun m -> m "Not found in cache");
     let value = thunk () in
     Hashtbl.add cache key value;
     value

let remove = Hashtbl.remove
