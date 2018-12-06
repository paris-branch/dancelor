type behaviour_when_set =
  Fail | Override | DoNothing

let generic_setter full_name ref =
  fun ?(when_set=Fail) x ->
  match !ref with
  | None -> ref := Some x
  | Some _ ->
     match when_set with
     | Fail -> failwith full_name
     | Override -> ref := Some x
     | DoNothing -> ()

let generic_getter full_name ref =
  fun () ->
  match !ref with
  | None -> failwith full_name
  | Some x -> x

(* =============================== [ Cache ] ================================ *)

let cache_prefix = ref None
let set_cache_prefix = generic_setter "Config.set_cache_prefix" cache_prefix
let cache_prefix = generic_getter "Config.cache_prefix" cache_prefix

(* =============================== [ Views ] ================================ *)

let views_prefix = ref None
let set_views_prefix = generic_setter "Config.set_views_prefix" views_prefix
let views_prefix = generic_getter "Config.views_prefix" views_prefix

(* ============================== [ Database ] ============================== *)

let database_prefix = ref None
let set_database_prefix = generic_setter "Config.set_database_prefix" database_prefix
let database_prefix = generic_getter "Config.database_prefix" database_prefix

(* Port *)

let port () = 8080

(* Main *)

let load_env_var setter var =
  try setter (Sys.getenv var)
  with Not_found -> Log.(debug_async (spf "Env var not found: %s" var))

let load_from_env ?when_set () =
  load_env_var (set_cache_prefix ?when_set) "DANCELOR_CACHE";
  load_env_var (set_views_prefix ?when_set) "DANCELOR_VIEWS";
  load_env_var (set_database_prefix ?when_set) "DANCELOR_DATABASE"

let load_from_cli ?when_set () =
  ignore when_set (* FIXME *)

let load () =
  Log.(info_async "Loading configuration");
  load_from_env ~when_set:Fail ();
  load_from_cli ~when_set:Override ()
  (* FIXME: check that all set *)
(* FIXME: Print config in logs *)

let () = load ()
