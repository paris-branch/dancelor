let preload_tunes () =
  let%lwt all = Dancelor_database.Tune.get_all () in
  Lwt_list.iter_s
    (fun tune ->
       let%lwt _ = Dancelor_server_controller.Tune.Png.render tune in
       Lwt.return ())
    all

let initialise () =
  Lwt.async preload_tunes
