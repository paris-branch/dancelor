open Nes
open Common

module Log = (val Logger.create "routine": Logs.LOG)

let preload_versions ?max_concurrency () =
  let%lwt all = Database.Version.get_all () in
  let%lwt t =
    NesLwt_unix.with_difftimeofday @@ fun () ->
    Lwt_stream.iter_n
      ?max_concurrency
      (fun version ->
        let%lwt tune = Model.Version.tune' version in
        let name = Model.Tune.one_name' tune in
        Log.debug (fun m -> m "Prerendering %s" (NEString.to_string name));
        let%lwt _ = Controller.Job.wait_ignore =<< Controller.Version.render_svg (Entry.value version) Model.VersionParameters.none RenderingParameters.none in
        let%lwt _ = Controller.Job.wait_ignore =<< Controller.Version.render_ogg (Entry.value version) Model.VersionParameters.none RenderingParameters.none in
        lwt_unit
      )
      (Lwt_stream.of_list all)
  in
  Log.info (fun m -> m "Finished prerendering all versions in %fs" t);
  lwt_unit

let initialise () =
  let max_concurrency = if !Config.heavy_routines then 8 else 1 in
  Lwt.async (fun () -> preload_versions ~max_concurrency ())
