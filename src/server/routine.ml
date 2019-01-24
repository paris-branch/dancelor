open Dancelor_model
let (>>=) = Lwt.bind

let rec preload_tunes () =
  Lwt_list.iter_s
    (fun (_, tune, version) ->
      Dancelor_controller.TuneVersion.Png.render (tune, version) >>= fun _ ->
      Lwt.return ())
    (Tune.Database.get_all ())
  >>= fun () ->
  Lwt_unix.sleep 60. >>= fun () ->
  preload_tunes ()

let initialise () =
  Lwt.async preload_tunes
