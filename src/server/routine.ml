open Nes
open Dancelor_common

module Log = (val Logs.src_log @@ Logs.Src.create "server.routine": Logs.LOG)

(** A stream containing all the versions of the database. It never ends, and
    instead returns all the versions again, after a delay of 10 minutes. *)
let all_versions =
  Lwt_stream.concat @@
  Lwt_stream.from @@ fun () ->
  Lwt_unix.sleep 600.;%lwt
  Log.debug (fun m -> m "Generating the list of all versions for pre-rendering");
  (some % Lwt_stream.of_list) <$> Database.Version.get_all ()

(** A stream of prerendering jobs for versions in the database. This
    contains only pending and failed jobs, the others do not need to
    run again. *)
let all_versions_prerendering_job =
  Lwt_stream.filter_map_s
    (fun version ->
      let run_with_params =
        match Model.Version.content' version with
        | No_content -> `No_need_to_run
        | Monolithic _ -> `Run_with_params None
        | Destructured {default_structure; _} -> `Run_with_params (some @@ Model.Version_parameters.make ~structure: default_structure ()) (* FIXME: at the moment, this requires us to keep this in sync with the client, when this should be the default *)
      in
      match run_with_params with
      | `No_need_to_run -> lwt_none
      | `Run_with_params params ->
        let%lwt job =
          Controller.Job.register_job ~add_pending: false
          <$> Controller.Version.render_snippets ?version_params: params (Entry.value version)
        in
        match !(job.state) with
        | Failed _ | Pending -> lwt_some job
        | _ -> lwt_none
    )
    all_versions

let run_jobs_from ~tag ~max_concurrency =
  Lwt_stream.iter_n
    ~max_concurrency
    (fun job ->
      try%lwt
        Log.debug (fun m -> m "run_jobs_from %s: %s" tag Controller.Job.(expr_val job.expr));
        Controller.Job.run_job job
      with
        | exn -> !(Lwt.async_exception_hook) exn; lwt_unit
    )

let initialiase_job_runners ~threads =
  (* NOTE: There used to be something fancy where we had exactly
     [threads] threads, and one would pick up pre-rendering jobs when
     there was no pending job. As it turns out, reading in several
     places from the same stream is not safe and can lead to
     duplicates. There are ways around it, but the amount of code that
     it produces is not worth the trouble. *)
  Lwt.async (fun () ->
    Lwt.join [
      run_jobs_from ~tag: "prerender" ~max_concurrency: 1 all_versions_prerendering_job;
      run_jobs_from ~tag: "pending" ~max_concurrency: threads Controller.Job.pending_jobs;
    ]
  )

let initialise () =
  initialiase_job_runners ~threads: (Config.get ()).routine_threads
