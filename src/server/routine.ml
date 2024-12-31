module Log = (val Dancelor_server_logs.create "routine": Logs.LOG)

let preload_versions ?max_concurrency () =
  let%lwt all = Dancelor_server_database.Version.get_all () in
  let all = Lwt_stream.of_list all in
  let%lwt t =
    NesLwt_unix.with_difftimeofday @@ fun () ->
    Lwt_stream.iter_n
      ?max_concurrency
      (fun version ->
         let%lwt tune = Dancelor_server_model.Version.tune version in
         let name = Dancelor_server_model.Tune.name tune in
         Log.debug (fun m -> m "Prerendering %s" name);
         let%lwt _ = Dancelor_server_controller.Version.Svg.render version in
         let%lwt _ = Dancelor_server_controller.Version.Ogg.render version in
         Lwt.return ()
      )
      all
  in
  Log.info (fun m -> m "Finished prerendering all versions in %fs" t);
  Lwt.return_unit

let initialise () =
  let max_concurrency = if !Dancelor_server_config.heavy_routines then 8 else 1 in
  Lwt.async (fun () -> preload_versions ~max_concurrency ())
