open NesUnix
open Common

module Log = (val Logger.create "controller.job": Logs.LOG)

type state =
  | Pending
  | Running of {process: Lwt_process.process_full; stderr: string list ref}
  | Failed of {status: Unix.process_status; logs: string list}
  | Succeeded of {path: string}

let is_failed = function Failed _ -> true | _ -> false

(** A type for Nix expressions. *)
type expr = Expr of string
let expr_val (Expr s) = s

(** An internal job. This is an actual job, producing potentially several
    artifacts. A {!JobId.t} corresponds to an {!job_and_file}, which is an
    internal job AND a specific artifact within this job. This allows building
    several things at the same time, while still giving a simple “one job = one
    file” interface to the client. *)
type job = {
  expr: expr;
  state: state ref;
}

(* NOTE: The following table and stream need to be kept in sync. *)
let job_of_expr : (expr, job) Hashtbl.t = Hashtbl.create 8
let (pending_jobs : job Lwt_stream.t), add_pending_job = Lwt_stream.create ()

(** An job and a file within that job. See comment for {!job}. *)
type job_and_file = {
  id: JobId.t; (** used to identify a job when communicating with the client *)
  job: job;
  file: string;
}

(** NOTE: This needs to be kept in sync with {!job_of_expr} and {!pending_jobs},
    namely each registered {!job_and_file} should point to {!job} that is
    registered in {!job_of_expr}. *)
let job_and_file_of_id : (JobId.t, job_and_file) Hashtbl.t = Hashtbl.create 8

let register_job (expr : expr) (file : string) : JobId.t Endpoints.Job.registration_response =
  let job_and_file =
    match Hashtbl.find_opt job_of_expr expr with
    | Some job when not (is_failed !(job.state)) ->
      (*  if there is a job for the same expression, but not necessarily the
          same file, we can just return without starting an actual job; we do
          not do so for failed job in case the failure was transient *)
      {id = JobId.create (); job; file}
    | _ ->
      (* otherwise, we really do have to register a new job *)
      let id = JobId.create () in
      let job = {expr; state = ref Pending} in
      (* NOTE: we use {!Hashtbl.replace} because {!Hashtbl.add} might keep failed jobs *)
      Hashtbl.replace job_of_expr expr job;
      add_pending_job (Some job);
      Log.debug (fun m -> m "Registered new job: %s" (expr_val expr));
      {id; job; file}
  in
  Hashtbl.add job_and_file_of_id job_and_file.id job_and_file;
  (* shortcut for when the job is already successful. this is not possible with
     new job, but will often happen with cache hits. it saves one network call
     by allowing the client to request the file immediately *)
  match !(job_and_file.job.state) with
  | Succeeded _ -> AlreadySucceeded job_and_file.id
  | _ -> Registered job_and_file.id

let run_job job =
  match !(job.state) with
  | Running _ -> invalid_arg "run_job: cannot start a job that is already started"
  | Failed _ -> invalid_arg "run_job: cannot start a job that has already failed"
  | Succeeded _ -> invalid_arg "run_job: cannot start a job that has already succeeded"
  | Pending ->
    let command = [|"nix-build"; "--no-link"; "--impure"; "--expr"; expr_val job.expr|] in
    let process = Lwt_process.open_process_full ("", command) in
    let stderr = ref [] in
    job.state := Running {process; stderr};
    Lwt_io.close process#stdin;%lwt
    Lwt.async (fun () ->
      let rec follow_stderr () =
        match%lwt Lwt_io.read_line_opt process#stderr with
        | None -> lwt_unit
        | Some line -> stderr := !stderr @ [line]; follow_stderr ()
      in
      follow_stderr ()
    );
    (* block until the job is done running *)
    let%lwt status = process#status in
    let%lwt stdout = Lwt_io.read process#stdout in
    let%lwt last_stderr = String.split_on_char '\n' <$> Lwt_io.(atomic read process#stderr) in
    let stderr = !stderr @ last_stderr in
    Log.debug (fun m -> m "Ran job: %s" (expr_val job.expr));
    Log.debug (fun m -> m "Status: %a" Process.pp_process_status status);
    Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stdout") stdout);
    Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stderr") (String.concat "\n" stderr));
    (
      job.state :=
        match status with
        | WEXITED 0 -> Succeeded {path = String.trim stdout}
        | _ -> Failed {status; logs = stderr}
    );
    lwt_unit

let make expr = lwt {expr; state = ref Pending}

let get id =
  match Hashtbl.find_opt job_and_file_of_id id with
  | None -> Madge_server.shortcut_not_found "This job does not exist anymore, or has never existed."
  | Some job_and_file -> lwt job_and_file

let status id =
  Log.debug (fun m -> m "status %s" (JobId.to_string id));
  get id >>= fun {job; _} ->
  lwt @@
    match !(job.state) with
    | Pending -> Endpoints.Job.Status.Pending
    | Running {stderr; _} -> Running !stderr
    | Failed {logs; _} -> Failed logs
    | Succeeded _ -> Succeeded

let file id slug =
  Log.debug (fun m -> m "file %s %s" (JobId.to_string id) (Entry.Slug.to_string slug));
  get id >>= fun {job; file; _} ->
  match !(job.state) with
  | Pending -> Madge_server.shortcut_bad_request "This job is not running yet, you cannot query the file."
  | Running _ -> Madge_server.shortcut_bad_request "This job is still running, you cannot query the file."
  | Failed _ -> Madge_server.shortcut_bad_request "This job failed, you cannot query the file."
  | Succeeded {path} -> Madge_server.respond_file ~fname: (Filename.concat path file)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Job.t -> a = fun _env endpoint ->
  match endpoint with
  | Status -> status
  | File -> file
