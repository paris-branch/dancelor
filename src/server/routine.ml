open Dancelor_model
let (>>=) = Lwt.bind

let preload_tunes () =
  Lwt_list.iter_s
    (fun tune ->
      Dancelor_controller.Tune.Png.render tune >>= fun _ ->
      Lwt.return ())
    (Tune.Database.get_all ())
  
let initialise () =
  Lwt.async preload_tunes
