open Nes open NesOption

let int = Json.int
let string = Json.string

let bool =
  Json.string >=> function
  | "true" | "yes" | "on" -> Some true
  | "false" | "no" | "off" -> Some false
  | _ -> None

let loglevel_of_string = function
  | "error" -> Logs.Error
  | "warning" -> Warning
  | "info" -> Info
  | "debug" -> Debug
  | _ -> assert false

let loglevel_of_json_string json =
  Json.string json >>= fun s ->
  Some (loglevel_of_string s)

let loglevel_to_string = function
  | Logs.Error -> "error"
  | Warning -> "warning"
  | Info -> "info"
  | Debug -> "debug"
  | App -> assert false

(* =========================== [ Dynamic Stuff ] ============================ *)

let cache = ref "cache"
let database = ref "database"
let init_only = ref false
let lilypond = ref "lilypond"
let loglevel = ref Logs.Debug
let port = ref 8080
let routines = ref true
let share = ref "share"
let sync_storage = ref true

let load_from_file filename =
  let config =
    try
      let ichan = open_in filename in
      let config =
        in_channel_to_string ichan
        |> Json.from_string
      in
      close_in ichan;
      config
    with
      exn -> raise exn
  in
  let field config ~type_ ~default path =
    match Json.(get_opt ~k:type_ path config) with
    | None -> default
    | Some value -> value
  in
  cache        := field config ~type_:string ~default:!cache        ["cache"];
  database     := field config ~type_:string ~default:!database     ["database"];
  init_only    := field config ~type_:bool   ~default:!init_only    ["init_only"];
  lilypond     := field config ~type_:string ~default:!lilypond     ["lilypond"];
  loglevel     := field config ~type_:loglevel_of_json_string ~default:!loglevel ["loglevel"];
  port         := field config ~type_:int    ~default:!port         ["port"];
  routines     := field config ~type_:bool   ~default:!routines     ["routines"];
  share        := field config ~type_:string ~default:!share        ["share"];
  sync_storage := field config ~type_:bool   ~default:!sync_storage ["sync_storage"];
  ()

let parse_cmd_line () =
  let open Arg in
  let specs = [
    "--cache",           Set_string cache,     spf "DIR Set cache directory (default: %s)"    !cache;
    "--config",          String load_from_file, spf "FILE Load configuration from FILE. This overrides all previous command-line settings but is overriden by the next ones.";
    "--database",        Set_string database,  spf "DIR Set database directory (default: %s)" !database;
    "--init-only",       Set        init_only, spf " Stop after initialisation%s" (if !init_only then " (default)" else "");
    "--no-init-only",    Clear      init_only, spf " Do not stop after initialisation%s" (if not !init_only then " (default)" else "");
    "--lilypond",        Set_string lilypond,  spf "PATH Set path to the Lilypond binary (default: %s)" !lilypond;
    "--loglevel",        String (loglevel_of_string ||> (:=) loglevel), spf "LEVEL Set the log level (default: %s)" (loglevel_to_string !loglevel);
    "--port",            Set_int    port,      spf "NB Set the port (default: %d)" !port;
    "--routines",        Set        routines,  spf " Start routines%s" (if !routines then " (default)" else "");
    "--no-routines",     Clear      routines,  spf " Do not start routines%s" (if not !routines then " (default)" else "");
    "--share",           Set_string share,     spf "DIR Set share directory (default: %s)" !share;
    "--sync-storage",    Set        sync_storage, spf " Sync storage using git%s" (if !sync_storage then " (default)" else "");
    "--no-sync-storage", Clear      sync_storage, spf " Do not sync storage using git%s" (if not !sync_storage then " (default)" else "");
  ]
  in
  let anon_fun _ = raise (Arg.Bad "no anonymous argument expected") in
  let usage = spf "Usage: %s [OPTIONS...]" Sys.argv.(0) in
  parse specs anon_fun usage