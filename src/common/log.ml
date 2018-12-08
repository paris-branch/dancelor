let create unit =
  let src = Logs.Src.create unit in
  Logs.Src.set_level src (Some Debug);
  Logs.src_log src

let my_reporter () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags fmt ->
    ignore tags; ignore header;
    let ppf = Format.err_formatter in
    let open Unix in
    let tm = Unix.(gettimeofday () |> localtime) in
    Format.kfprintf k ppf ("@[%d-%d-%d %d:%d:%d [%s] %s | " ^^ fmt ^^ "@]@.")
      (1900+tm.tm_year) (1+tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
      (String.uppercase_ascii (Logs.level_to_string (Some level))) (Logs.Src.name src)
  in
  { Logs.report }

let () = Logs.(set_reporter (my_reporter ()))

let () = Logs.set_level ~all:false None
