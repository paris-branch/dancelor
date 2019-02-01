let create unit =
  let src = Logs.Src.create unit in
  Logs.Src.set_level src (Some !Config.loglevel);
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
    Format.kfprintf k ppf ("@[<h 2>%s%04d-%02d-%02d %02d:%02d:%02d [%s] %s | " ^^ fmt ^^ "\027[0m@]@.")
      (level_to_color level)
      (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
      (String.uppercase_ascii (Logs.level_to_string (Some level))) (Logs.Src.name src)
  in
  { Logs.report }

let () = Logs.(set_reporter (my_reporter ()))

let () = Logs.set_level ~all:false None
