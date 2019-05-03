let (>>=) = Lwt.bind

let preload_tunes () =
  Dancelor_database.Tune.get_all ()
  |> List.of_seq
  |> Lwt_list.iter_s
    (fun tune ->
       Dancelor_server_controller.Tune.Png.render tune >>= fun _ ->
       Lwt.return ())

let initialise () =
  Lwt.async preload_tunes
