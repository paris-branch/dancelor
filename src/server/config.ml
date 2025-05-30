open Nes
module Log = (val Logger.create "config": Logs.LOG)

let int = Json.int
let string = Json.string

let bool x =
  Option.bind
    (Json.string x)
    (function
      | "true" | "yes" | "on" -> Some true
      | "false" | "no" | "off" -> Some false
      | _ -> None
    )

let loglevel_of_string = function
  | "error" -> Logs.Error
  | "warning" -> Warning
  | "info" -> Info
  | "debug" -> Debug
  | _ -> assert false

let loglevel_of_json_string json =
  Option.bind
    (Json.string json)
    (some % loglevel_of_string)

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
let pid_file = ref ""
let port = ref 6872
let routines = ref true
let heavy_routines = ref false
let share = ref "share"
let sync_storage = ref true
let write_storage = ref true
let github_token = ref ""
let github_token_file = ref ""
let github_repository = ref ""
let github_database_repository = ref ""

let check_config () =
  if !github_token_file <> "" then
    (
      if !github_token <> "" then
        failwith "Cannot specify both a GitHub token and a GitHub token file";
      let%lwt ichan = Lwt_io.open_file ~mode: Lwt_io.input !github_token_file in
      let%lwt content = Lwt_io.read ichan in
      github_token := String.trim content;
      lwt_unit
    )
  else
    lwt_unit

let load_from_file filename =
  Log.debug (fun m -> m "Reading configuration from file %s" filename);
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
      | Sys_error _ ->
        Log.err (fun m -> m "Could not find config file \"%s\"" filename);
        Logger.log_die (module Log)
  in
  let field config ~type_ ~default path =
    match Json.(get_opt ~k: type_ path config) with
    | None -> default
    | Some value -> value
  in
  cache := field config ~type_: string ~default: !cache ["cache"];
  database := field config ~type_: string ~default: !database ["database"];
  init_only := field config ~type_: bool ~default: !init_only ["init_only"];
  lilypond := field config ~type_: string ~default: !lilypond ["lilypond"];
  loglevel := field config ~type_: loglevel_of_json_string ~default: !loglevel ["loglevel"];
  pid_file := field config ~type_: string ~default: !pid_file ["pid_file"];
  port := field config ~type_: int ~default: !port ["port"];
  routines := field config ~type_: bool ~default: !routines ["routines"];
  heavy_routines := field config ~type_: bool ~default: !heavy_routines ["heavy_routines"];
  share := field config ~type_: string ~default: !share ["share"];
  sync_storage := field config ~type_: bool ~default: !sync_storage ["sync_storage"];
  write_storage := field config ~type_: bool ~default: !write_storage ["write_storage"];
  github_token := field config ~type_: string ~default: !github_token ["github-token"];
  github_token_file := field config ~type_: string ~default: !github_token_file ["github-token-file"];
  github_repository := field config ~type_: string ~default: !github_repository ["github-repository"];
  github_database_repository := field config ~type_: string ~default: !github_database_repository ["github-database-repository"];
  ()

let parse_cmd_line () =
  let open Arg in
  let pp_default fmt = function
    | true -> fpf fmt " (default)"
    | false -> ()
  in
  let specs =
    align
      [
        "--cache", Set_string cache, spf "DIR Set cache directory (default: %s)" !cache;
        "--config", String load_from_file, spf "FILE Load configuration from FILE. This overrides all previous command-line settings but is overriden by the next ones.";
        "--database", Set_string database, spf "DIR Set database directory (default: %s)" !database;
        "--init-only", Set init_only, aspf " Stop after initialisation%a" pp_default !init_only;
        "--no-init-only", Clear init_only, aspf " Do not stop after initialisation%a" pp_default (not !init_only);
        "--lilypond", Set_string lilypond, spf "PATH Set path to the LilyPond binary (default: %s)" !lilypond;
        "--loglevel", String (loglevel_of_string ||> (:=) loglevel), spf "LEVEL Set the log level (default: %s)" (loglevel_to_string !loglevel);
        "--pid-file", Set_string pid_file, spf "FILE Write process id to the given file.";
        "--port", Set_int port, spf "NB Set the port (default: %d)" !port;
        "--routines", Set routines, aspf " Start routines%a" pp_default !routines;
        "--no-routines", Clear routines, aspf " Do not start routines%a" pp_default (not !routines);
        "--heavy-routines", Set heavy_routines, aspf " Allow heavy load for routines%a" pp_default !heavy_routines;
        "--no-heavy-routines", Clear heavy_routines, aspf " Disallow heavy load for routines%a" pp_default (not !heavy_routines);
        "--share", Set_string share, spf "DIR Set share directory (default: %s)" !share;
        "--sync-storage", Set sync_storage, aspf " Sync storage using git%a" pp_default !sync_storage;
        "--no-sync-storage", Clear sync_storage, aspf " Do not sync storage using git%a" pp_default (not !sync_storage);
        "--write-storage", Set write_storage, aspf " Reflect storage on filesystem%a" pp_default !write_storage;
        "--no-write-storage", Clear write_storage, aspf " Do not reflect storage on filesystem%a" pp_default (not !write_storage);
        "--github-token", Set_string github_token, spf "STR Set the GitHub API token to STR. This is used by the issue report mechanism and must therefore be allowed to open issues on the repositories.";
        "--github-token-file", Set_string github_token_file, spf "FILE Read the GitHub API token from FILE. This is used by the issue report mechanism and must therefore be allowed to open issues on the repositories.";
        "--github-repository", Set_string github_repository, spf "STR Set the Github repository to STR. This is used by the issue report mechanism. It must contain the host, owner, and repository.";
        "--github-database-repository", Set_string github_database_repository, spf "STR Set the Github database repository to STR. This is used by the issue report mechanism. It must contain the host, owner, and repository.";
      ]
  in
  let anon_fun _ = raise (Arg.Bad "no anonymous argument expected") in
  let usage = spf "Usage: %s [OPTIONS...]" Sys.argv.(0) in
  parse specs anon_fun usage;
  check_config ()
