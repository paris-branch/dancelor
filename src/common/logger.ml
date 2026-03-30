open Nes

let log_src = Logs.Src.create "common.logger"
module Log = (val Logs.src_log log_src: Logs.LOG)

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

let setup_reporter ~on_message ~colors =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    on_message level src;
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags fmt ->
    ignore tags;
    ignore header;
    let ppf = Format.err_formatter in
    Format.kfprintf
      k
      ppf
      ("@[<h 2>%s%s %s | " ^^ fmt ^^ (if colors then "\027[0m" else "") ^^ "@]@.")
      (if colors then level_to_color level else "")
      (level_to_string level)
      src
  in
  Logs.set_reporter {report}

let initialise loglevel =
  (* Crawl through all defined sources, but handle the one for this module
     before all others, since it itself logs debug messages. *)
  List.iter
    (fun src ->
      let name = Logs.Src.name src in
      let level = find_level loglevel name in
      Logs.Src.set_level src level;
      Log.debug (fun m -> m "Set %S to %S" name (Logs.level_to_string level))
    )
    (log_src :: Logs.Src.list ());
  (* Initialise future loglevels to the default. *)
  Logs.set_level ~all: false loglevel.default

let early_loglevel = {cases = []; default = Some Info}

(** Early initialisation. This has to be called before anything else, so that
    even if early things break, we get some logging. *)
let early_initialisation ~on_message ~colors =
  Log.debug (fun m -> m "Starting early initialisation of logging...");
  setup_reporter ~on_message ~colors;
  initialise early_loglevel;
  Log.info (fun m -> m "Early initialisation of logging done")

(** Late initialisation. This is meant to be called after eg. configuration has
    been loaded, to set up the right log levels. *)
let late_initialisation loglevel =
  Log.debug (fun m -> m "Starting late initialisation of logging...");
  initialise loglevel;
  Log.info (fun m -> m "Late initialisation of logging done")

(** Full initialisation, for when it doesn't make sense to separate between
    early and late phases, eg. on the JS client. *)
let full_initialisation ~on_message ~colors loglevel =
  Log.debug (fun m -> m "Starting full initialisation of logging...");
  setup_reporter ~on_message ~colors;
  initialise loglevel;
  Log.info (fun m -> m "Full initialisation of logging done")
