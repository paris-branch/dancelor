open Nes

module type LOG = Logs.LOG

let log_src = Logs.Src.create "server.logger"
module Log = (val Logs.src_log log_src: LOG)

let level_to_string = function
  | Logs.Debug -> "DBG"
  | Info -> "INF"
  | Warning -> "WRN"
  | Error -> "ERR"
  | App -> "APP"

let level_to_color = function
  | Logs.Debug -> "\027[37m" (* gray *)
  | Info -> "" (* white *)
  | Warning -> "\027[33m" (* yellow *)
  | Error -> "\027[31m" (* red *)
  | App -> "\027[1m" (* white bold *)

let my_reporter () =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    Prometheus_unix.Logging.inc_counter level src;
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags fmt ->
    ignore tags;
    ignore header;
    let ppf = Format.err_formatter in
    let time = Unix.(gettimeofday () |> localtime) in
    Format.kfprintf
      k
      ppf
      ("@[<h 2>%s%02d:%02d:%02d %s %s | " ^^ fmt ^^ "\027[0m@]@.")
      (level_to_color level)
      time.tm_hour
      time.tm_min
      time.tm_sec
      (level_to_string level)
      src
  in
    {Logs.report}

type loglevel_map = {
  cases: (string * Logs.level option) list;
  default: Logs.level option;
}

let matches_loglevel_pattern ~pattern name =
  let pattern = String.split_on_char '.' pattern in
  let name = String.split_on_char '.' name in
  let rec matches pattern name =
    match pattern, name with
    | [], [] -> true
    | ["*"], _ -> true
    | "*" :: _, _ -> invalid_arg "matches_loglevel_pattern: invalid pattern"
    | p :: ps, n :: ns when p = n -> matches ps ns
    | _ -> false
  in
  matches pattern name

let find_level loglevel_map name =
  match List.find_opt (fun (pattern, _) -> matches_loglevel_pattern ~pattern name) loglevel_map.cases with
  | Some (_, level) -> level
  | None -> loglevel_map.default

let update_past_loglevel loglevel_map =
  (* Crawl through all defined sources, but handle the one for this module
     before all others, since it itself logs debug messages. *)
  List.iter
    (fun src ->
      let name = Logs.Src.name src in
      let level = find_level loglevel_map name in
      Logs.Src.set_level src level;
      Log.debug (fun m -> m "Set %S to %S" name (Logs.level_to_string level))
    )
    (log_src :: Logs.Src.list ())

let initialise_future_loglevel loglevel =
  Logs.set_level ~all: false loglevel

let () =
  Prometheus_unix.Logging.init ();
  (* This has to be done before anything else, so that even if early things
     break, we get some logging. The logging level will then be possibly changed
     by the configuration. *)
  Logs.(set_reporter (my_reporter ()));
  update_past_loglevel {cases = []; default = Some Logs.Info};
  initialise_future_loglevel (Some Logs.Info);
  Log.info (fun m -> m "Early initialisation of logging done")

let initialise loglevel =
  Log.info (fun m -> m "Initialise logging");
  update_past_loglevel loglevel;
  initialise_future_loglevel loglevel.default

let log_exit (module Log : LOG) n =
  Log.info (fun m -> m "Exiting with return code %d" n);
  (* no need to flush as all logging messages are flushed individually already *)
  exit n

let log_die (module Log : LOG) =
  log_exit (module Log) 1
