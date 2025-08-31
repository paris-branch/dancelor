open Nes
open Common

module Log = (val Logger.create "routine": Logs.LOG)

(** A stream containing all the versions of the database. It never ends, and
    instead returns all the versions again, after a delay of 10 minutes. *)
let all_versions =
  Lwt_stream.concat @@
  Lwt_stream.from @@ fun () ->
  Lwt_unix.sleep 600.;%lwt
  some % Lwt_stream.of_list <$> Database.Version.get_all ()

let all_versions_prerendering_job =
  Lwt_stream.concat @@
    Lwt_stream.map_s
      (fun version ->
        let%lwt name = Model.Version.one_name' version in
        Log.debug (fun m -> m "Prerendering %s" (NEString.to_string name));
        let%lwt render_svg_job = Controller.Version.render_svg (Entry.value version) in
        let%lwt render_ogg_job = Controller.Version.render_ogg (Entry.value version) in
        lwt @@ Lwt_stream.of_list [render_svg_job; render_ogg_job]
      )
      all_versions

let run_all_jobs ?max_concurrency () =
  Lwt_stream.iter_n
    ?max_concurrency
    (fun job ->
      try%lwt
        Controller.Job.run_job job
      with
        | exn -> !(Lwt.async_exception_hook) exn; lwt_unit
    )
    (
      Lwt_stream.choose_biased [
        Controller.Job.pending_jobs;
        all_versions_prerendering_job;
      ]
    )

let initialise () =
  Lwt.async (fun () -> run_all_jobs ~max_concurrency: 2 ())
