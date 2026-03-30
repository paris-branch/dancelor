open Nes

module Log = (val Logs.src_log @@ Logs.Src.create "server.config": Logs.LOG)

type t = {
  database: string;
  init_only: bool;
  loglevel: Common.Logger.loglevel_map;
  pid_file: string;
  port: int;
  share: string;
  sync_storage: bool;
  write_storage: bool;
  github_token: string;
  github_token_file: string;
  github_repository: string;
  github_database_repository: string;
  nixpkgs: string;
  routine_threads: int;
}
[@@deriving show {with_path = false}, yojson]

let set, unsafe_set, get =
  let v : t option ref = ref None in
  let get () = match !v with None -> failwith "Configuration not set" | Some config -> config in
  let unsafe_set config = v := Some config in
  let set config = match !v with Some _ -> failwith "Cannot set configuration twice" | None -> v := Some config in
  set, unsafe_set, get

let load_from_file filename =
  Log.debug (fun m -> m "Reading configuration from file %s" filename);
  let%lwt content = Lwt_io.with_file ~flags: [O_RDONLY] ~mode: Lwt_io.input filename Lwt_io.read in
  let content = Yojson.Safe.from_string content in
  let config = match of_yojson content with Ok config -> config | Error msg -> failwithf "Could not parse config file: %s" msg in
  let%lwt config =
    match config.github_token, config.github_token_file with
    | "", "" ->
      Log.warn (fun m -> m "No GitHub token provided; some features may not work.");
      lwt config
    | "", _ ->
      Log.info (fun m -> m "Reading GitHub token from file...");
      let%lwt token = Lwt_io.with_file ~flags: [O_RDONLY] ~mode: Lwt_io.input config.github_token_file Lwt_io.read in
      lwt {config with github_token = String.trim token}
    | _, "" ->
      lwt config
    | _, _ ->
      failwith "Both GitHub token and GitHub token file provided. Please provide only one of them."
  in
  set config;
  Log.info (fun m -> m "Loaded configuration:@\n@[<2>%a@]" pp config);
  lwt_unit

let parse_cmd_line () =
  match Sys.argv with
  | [|_; fname|] -> load_from_file fname
  | [|_|] -> failwith "No config file specified. Please provide a config file as a command-line argument."
  | _ -> failwith "Too many command-line arguments. Expected exactly one: the config file."
