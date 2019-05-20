module Log = (val Dancelor_server_logs.create "routine" : Logs.LOG)

let preload_tunes () =
  let%lwt all = Dancelor_server_database.Tune.get_all () in
  Lwt_list.iter_s
    (fun tune ->
       let%lwt group = Dancelor_server_model.Tune.group tune in
       let%lwt name = Dancelor_server_model.TuneGroup.name group in
       Log.debug (fun m -> m "Prerendering %s" name);
       let%lwt _ = Dancelor_server_controller.Tune.Png.render tune in
       Lwt.return ())
    all

let initialise () =
  Lwt.async preload_tunes
