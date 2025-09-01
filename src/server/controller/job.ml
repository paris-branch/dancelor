open NesUnix
open Common

module Log = (val Logger.create "controller.job": Logs.LOG)

type state =
  | Pending
  | Running of {process: Lwt_process.process_full; stderr: string list ref}
  | Failed of {status: Unix.process_status; logs: string list}
  | Succeeded of {path: string}

type t = {
  expr: string;
  state: state ref;
}

(* NOTE: The following table and stream need to be kept in sync. *)
let job_of_id : (JobId.t, t) Hashtbl.t = Hashtbl.create 8
let job_of_expr : (string, (JobId.t * t)) Hashtbl.t = Hashtbl.create 8
let (pending_jobs : (JobId.t * t) Lwt_stream.t), add_pending_job = Lwt_stream.create ()

let register_job job =
  match Hashtbl.find_opt job_of_expr job.expr with
  | Some (id, _) -> id
  | None ->
    let id = JobId.create () in
    Log.debug (fun m -> m "Registering job %s:@\n%s" (JobId.to_string id) job.expr);
    Hashtbl.add job_of_id id job;
    Hashtbl.add job_of_expr job.expr (id, job);
    add_pending_job (Some (id, job));
    id

type output = {out: string} [@@deriving of_yojson]
type result = {outputs: output list} [@@deriving of_yojson]

let run_job id job =
  match !(job.state) with
  | Running _ -> invalid_arg "run_job: cannot start a job that is already started"
  | Failed _ -> invalid_arg "run_job: cannot start a job that has already failed"
  | Succeeded _ -> invalid_arg "run_job: cannot start a job that has already succeeded"
  | Pending ->
    Log.debug (fun m -> m "Running job %s:@\n%s" (JobId.to_string id) job.expr);
    let command = [|
      "nix";
      "--extra-experimental-features";
      "nix-command";
      "build";
      "--print-build-logs";
      "--verbose";
      "--log-format";
      "raw";
      "--json";
      "--no-link";
      "--impure";
      "--expr";
      job.expr
    |]
    in
    let process = Lwt_process.open_process_full ("", command) in
    let stderr = ref [] in
    job.state := Running {process; stderr};
    Lwt_io.close process#stdin;%lwt
    Lwt.async (fun () ->
      let rec follow_stderr () =
        match%lwt Lwt_io.read_line_opt process#stderr with
        | None -> lwt_unit
        | Some line ->
          Log.debug (fun m -> m "%s> %s" (JobId.to_string id) line);
          stderr := !stderr @ [line];
          follow_stderr ()
      in
      follow_stderr ()
    );
    (* block until the job is done running *)
    let%lwt status = process#status in
    let%lwt stdout = Lwt_io.read process#stdout in
    let%lwt last_stderr = String.split_on_char '\n' <$> Lwt_io.(atomic read process#stderr) in
    let stderr = !stderr @ last_stderr in
    Log.debug (fun m -> m "Ran job %s:@\n%s" (JobId.to_string id) job.expr);
    Log.debug (fun m -> m "Status: %a" Process.pp_process_status status);
    Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stdout") stdout);
    Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stderr") (String.concat "\n" stderr));
    (
      job.state :=
        match status with
        | WEXITED 0 ->
          (
            match Yojson.Safe.from_string stdout with
            (* two cases depending on whether there is `startTime` and `endTime` or not *)
            | `List [`Assoc [_; ("outputs", `Assoc [("out", `String path)]); _; _]]
            | `List [`Assoc [_; ("outputs", `Assoc [("out", `String path)])]] ->
              Succeeded {path}
            | _ ->
              Failed {status; logs = stderr @ ["Could not parse output:"; stdout]}
          )
        | _ -> Failed {status; logs = stderr}
    );
    lwt_unit

let make expr = lwt {expr; state = ref Pending}

let get id =
  match Hashtbl.find_opt job_of_id id with
  | None -> Madge_server.shortcut_not_found "This job does not exit anymore, or has never existed."
  | Some job -> lwt job

let status id =
  Log.debug (fun m -> m "status %s" (JobId.to_string id));
  get id >>= fun job ->
  lwt @@
    match !(job.state) with
    | Pending -> Endpoints.Job.Status.Pending
    | Running {stderr; _} -> Running !stderr
    | Failed {logs; _} -> Failed logs
    | Succeeded _ -> Succeeded

let file id _slug =
  Log.debug (fun m -> m "status %s" (JobId.to_string id));
  get id >>= fun job ->
  match !(job.state) with
  | Pending -> Madge_server.shortcut_bad_request "This job is not running yet, you cannot query the file."
  | Running _ -> Madge_server.shortcut_bad_request "This job is still running, you cannot query the file."
  | Failed _ -> Madge_server.shortcut_bad_request "This job failed, you cannot query the file."
  | Succeeded {path} -> Madge_server.respond_file ~fname: path

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Job.t -> a = fun _env endpoint ->
  match endpoint with
  | Status -> status
  | File -> file
