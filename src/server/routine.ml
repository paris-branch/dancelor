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
  Lwt_stream.map_s
    (fun version ->
      let%lwt name = Model.Version.one_name' version in
      Log.debug (fun m -> m "Prerendering snippets for version %s" (NEString.to_string name));
      let%lwt render_snippets_expr = Controller.Version.render_snippets (Entry.value version) in
      lwt
        Controller.Job.{
          expr = render_snippets_expr;
          state = ref Pending
        }
    )
    all_versions

let run_jobs_from ~max_concurrency =
  Lwt_stream.iter_n
    ~max_concurrency
    (fun job ->
      try%lwt
        Controller.Job.run_job job
      with
        | exn -> !(Lwt.async_exception_hook) exn; lwt_unit
    )

let initialiase_job_runners ~threads =
  assert (threads >= 2);
  Lwt.async (fun () ->
    (* Have one thread pick jobs from the version prerendering queue when
       there is nothing in the pending jobs queue. *)
    run_jobs_from
      ~max_concurrency: 1
      (
        Lwt_stream.choose_biased [
          Controller.Job.pending_jobs;
          all_versions_prerendering_job;
        ]
      )
  );
  Lwt.async (fun () ->
    (* The others (at least one) are fully dedicated to user jobs. *)
    run_jobs_from
      ~max_concurrency: (threads - 1)
      Controller.Job.pending_jobs
  )

let initialise () =
  initialiase_job_runners ~threads: 2
