module Config = Dancelor_server_config
module Database = Dancelor_server_database
module Model = Dancelor_server_model
module Controller = Dancelor_server_controller

module Log = (val Dancelor_server_logs.create "routine": Logs.LOG)

let preload_versions ?max_concurrency () =
  let%lwt all = Database.Version.get_all () in
  let all = Lwt_stream.of_list all in
  let%lwt t =
    NesLwt_unix.with_difftimeofday @@ fun () ->
    Lwt_stream.iter_n
      ?max_concurrency
      (fun version ->
         let%lwt tune = Model.Version.tune version in
         let name = Model.Tune.name tune in
         Log.debug (fun m -> m "Prerendering %s" name);
         let%lwt _ = Controller.Version.Svg.render Model.VersionParameters.none version in
         let%lwt _ = Controller.Version.Ogg.render Model.VersionParameters.none version in
         Lwt.return ()
      )
      all
  in
  Log.info (fun m -> m "Finished prerendering all versions in %fs" t);
  Lwt.return_unit

let initialise () =
  let max_concurrency = if !Config.heavy_routines then 8 else 1 in
  Lwt.async (fun () -> preload_versions ~max_concurrency ())
