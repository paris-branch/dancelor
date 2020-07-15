module Log = (val Dancelor_server_logs.create "routine" : Logs.LOG)

let preload_tunes ?max_concurrency () =
  let%lwt all = Dancelor_server_database.Tune.get_all () in
  let all = Lwt_stream.of_list all in
  let%lwt t =
    NesLwt_unix.with_difftimeofday @@ fun () ->
    Lwt_stream.iter_n
      ?max_concurrency
      (fun tune ->
         let%lwt group = Dancelor_server_model.Tune.group tune in
         let%lwt name = Dancelor_server_model.TuneGroup.name group in
         Log.debug (fun m -> m "Prerendering %s" name);
         let%lwt _ = Dancelor_server_controller.Tune.Svg.render tune in
         Lwt.return ())
      all
  in
  Log.info (fun m -> m "Finished prerendering all tunes in %fs" t);
  Lwt.return_unit

let initialise () =
  let max_concurrency = if !Dancelor_server_config.heavy_routines then 8 else 1 in
  Lwt.async (fun () -> preload_tunes ~max_concurrency ())
