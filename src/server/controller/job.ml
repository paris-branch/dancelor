open NesUnix
open Common

module Log = (val Logger.create "controller.job": Logs.LOG)

type command = string array [@@deriving show]

type process_option = Pending | Running of Lwt_process.process_full | Exited of Unix.process_status

type t = {
  command: command;
  process: process_option ref;
  stdout: string ref;
  stderr: string list ref;
  files: string list; (* files involved in the job - can be removed at the end *)
}

let status job =
  match !(job.process) with
  | Pending -> lwt ("", Endpoints.Job.Response.pending)
  | Running _ -> lwt ("", {Endpoints.Job.Response.status = Running; log_lines = !(job.stderr)})
  | Exited status ->
    Lwt_list.iter_p
      (fun fname ->
        try%lwt
          Lwt_unix.unlink fname
        with
          | Unix.(Unix_error (ENOENT, _, _)) -> lwt_unit
      )
      job.files;%lwt
    lwt (
      !(job.stdout),
      Endpoints.Job.Response.{
        status = if status = WEXITED 0 then Succeeded else Failed;
        log_lines = !(job.stderr)
      }
    )

let jobs : (JobId.t, t) Hashtbl.t = Hashtbl.create 8
let (pending_jobs : (JobId.t * t) Lwt_stream.t), add_pending_job = Lwt_stream.create ()

let register_job job =
  let id = JobId.create () in
  Log.debug (fun m -> m "Registering job %s:@\n%a" (JobId.to_string id) pp_command job.command);
  add_pending_job (Some (id, job));
  Hashtbl.add jobs id job;
  id

let run_job id job =
  match !(job.process) with
  | Exited _ -> invalid_arg "run_job: cannot start a job that is already finished"
  | Running _ -> invalid_arg "run_job: cannot start a job that is already started"
  | Pending ->
    Log.debug (fun m -> m "Running job %s:@\n%a" (JobId.to_string id) pp_command job.command);
    let process = Lwt_process.open_process_full ("", job.command) in
    job.process := Running process;
    Lwt_io.close process#stdin;%lwt
    Lwt.async (fun () ->
      let rec follow_stderr_lines () =
        match%lwt Lwt_io.read_line_opt process#stderr with
        | None -> lwt_unit
        | Some line ->
          Log.debug (fun m -> m "%s> %s" (JobId.to_string id) line);
          job.stderr := !(job.stderr) @ [line];
          follow_stderr_lines ()
      in
      follow_stderr_lines ()
    );
    (* block until the job is done running *)
    let%lwt status = process#status in
    job.process := Exited status;
    let%lwt stdout = Lwt_io.read process#stdout in
    job.stdout := stdout;
    let%lwt last_lines = String.split_on_char '\n' <$> Lwt_io.(atomic read process#stderr) in
    job.stderr := !(job.stderr) @ last_lines;
    Log.debug (fun m -> m "Ran job %s:@\n%a" (JobId.to_string id) pp_command job.command);
    Log.debug (fun m -> m "Status: %a" Process.pp_process_status status);
    Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stdout") stdout);
    Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stderr") (String.concat "\n" !(job.stderr)));
    lwt_unit

let nix_build_job ?(files = []) expr =
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
    expr
  |]
  in
  let process = ref Pending in
  lwt {command; process; stdout = ref ""; stderr = ref []; files}

let get id =
  match Hashtbl.find_opt jobs id with
  | None -> Madge_server.shortcut_not_found "This job does not exit anymore, or has never existed."
  | Some job -> lwt job

let status id = status =<< get id

type output = {out: string} [@@deriving of_yojson]
type result = {outputs: output list} [@@deriving of_yojson]

let file id _slug =
  let%lwt (stdout, response) = status id in
  match response.status with
  | Pending -> Madge_server.shortcut_bad_request "This job is not running yet, you cannot query the file."
  | Running -> Madge_server.shortcut_bad_request "This job is still running, you cannot query the file."
  | Failed -> Madge_server.shortcut_bad_request "This job failed, you cannot query the file."
  | Succeeded ->
    match Yojson.Safe.from_string stdout with
    (* two cases depending on whether there is `startTime` and `endTime` or not *)
    | `List [`Assoc [_; ("outputs", `Assoc [("out", `String fname)]); _; _]]
    | `List [`Assoc [_; ("outputs", `Assoc [("out", `String fname)])]] ->
      Madge_server.respond_file ~fname
    | _ -> assert false

let status id =
  Log.debug (fun m -> m "status %s" (JobId.to_string id));
  snd <$> status id

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Job.t -> a = fun _env endpoint ->
  match endpoint with
  | Status -> status
  | File -> file
