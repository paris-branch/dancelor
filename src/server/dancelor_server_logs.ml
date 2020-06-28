open Nes

let log_src = Logs.Src.create "dancelor.server.logs"
module Log = (val Logs.src_log log_src : Logs.LOG)

let create unit =
  Log.debug (fun m -> m "Creating log unit dancelor.server.%s" unit);
  let src = Logs.Src.create ("dancelor.server." ^ unit) in
  Logs.src_log src

let level_to_color = function
  | Logs.Debug -> "\027[37m" (* gray *)
  | Info -> ""               (* white *)
  | Warning -> "\027[33m"    (* yellow *)
  | Error -> "\027[31m"      (* red *)
  | App -> "\027[1m"         (* white bold *)

let my_reporter () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags fmt ->
    ignore tags; ignore header;
    let ppf = Format.err_formatter in
    let open Unix in
    let tm = Unix.(gettimeofday () |> localtime) in
    Format.kfprintf k ppf ("@[<h 2>%s%04d-%02d-%02d %02d:%02d:%02d | %s | %s | " ^^ fmt ^^ "\027[0m@]@.")
      (level_to_color level)
      (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
      (String.uppercase_ascii (Logs.level_to_string (Some level))) (Logs.Src.name src)
  in
  { Logs.report }

let initialise_reporter () =
  Logs.(set_reporter (my_reporter ()))

let update_past_loglevel () =
  (* Crawl through all defined sources, handle the one for this module before
     all others. *)
  (log_src :: Logs.Src.list ()) |> List.iter @@ fun src ->
  let name = Logs.Src.name src in
  let level =
    (* If the source comes from us, set loglevel to the given one. Otherwise,
       set to None. *)
    if    name = "dancelor" || String.starts_with ~needle:"dancelor." name
       || name = "lilypond" || String.starts_with ~needle:"lilypond." name
       || name = "nes"      || String.starts_with ~needle:"nes." name
    then
      Some !Dancelor_server_config.loglevel
    else
      None
  in
  Logs.Src.set_level src level;
  Log.debug (fun m -> m "Set '%s' to '%s'" name (Logs.level_to_string level))

let initialise_future_loglevel () =
  Logs.set_level ~all:false (Some !Dancelor_server_config.loglevel)

let initialise () =
  initialise_reporter ();
  update_past_loglevel ();
  initialise_future_loglevel ()
