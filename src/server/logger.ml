open Nes

module type LOG = Logs.LOG

let log_src = Logs.Src.create "server.logger"
module Log = (val Logs.src_log log_src: LOG)

let create unit =
  let unit =
    match unit with
    | "" -> "server"
    | _ -> "server." ^ unit
  in
  Log.debug (fun m -> m "Creating log unit %s" unit);
  Logs.src_log @@ Logs.Src.create unit

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
      (Logs.Src.name src)
  in
  {Logs.report}

let update_past_loglevel loglevel =
  (* Crawl through all defined sources, handle the one for this module before
     all others. *)
  (log_src :: Logs.Src.list ())
  |> List.iter @@ fun src ->
  let name = Logs.Src.name src in
  let level =
    (* If the source comes from us, set loglevel to the given one. Otherwise,
       set to None. *)
    if name = "server"
    || String.starts_with ~needle: "server." name
    || name = "lilypond"
    || String.starts_with ~needle: "lilypond." name
    || name = "nes"
    || String.starts_with ~needle: "nes." name
    || name = "madge"
    || String.starts_with ~needle: "madge." name then
      Some loglevel
    else
      None
  in
  Logs.Src.set_level src level;
  Log.debug (fun m -> m "Set '%s' to '%s'" name (Logs.level_to_string level))

let initialise_future_loglevel loglevel =
  Logs.set_level ~all: false (Some loglevel)

let () =
  (* This has to be done before anything else, so that even if early things
     break, we get some logging. The logging level will then be possibly changed
     by the configuration. *)
  Logs.(set_reporter (my_reporter ()));
  update_past_loglevel Logs.Info;
  initialise_future_loglevel Logs.Info;
  Log.info (fun m -> m "Early initialisation of logging done")

let initialise loglevel =
  Log.info (fun m -> m "Initialise logging");
  update_past_loglevel loglevel;
  initialise_future_loglevel loglevel

let log_exit (module Log : LOG) n =
  Log.info (fun m -> m "Exiting with return code %d" n);
  (* no need to flush as all logging messages are flushed individually already *)
  exit n

let log_die (module Log : LOG) =
  log_exit (module Log) 1
