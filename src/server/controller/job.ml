open NesUnix
open Common

module Log = (val Logger.create "controller.job": Logs.LOG)

type command = string array [@@deriving show]

type process_option = Pending | Started of Lwt_process.process_full

type t = {
  id: JobId.t;
  command: command;
  process: process_option ref;
  stdout: Buffer.t;
  stderr: Buffer.t;
  files: string list; (* files involved in the job - can be removed at the end *)
}

let id job = job.id

let rec read_in_channel_into_buffer ichan buf =
  let%lwt chunk = Lwt_io.read ~count: 1024 ichan in
  Buffer.add_string buf chunk;
  match String.length chunk with
  | 1024 -> read_in_channel_into_buffer ichan buf
  | _ -> lwt_unit

let status id job =
  let read_outputs process =
    read_in_channel_into_buffer process#stdout job.stdout;%lwt
    read_in_channel_into_buffer process#stderr job.stderr;%lwt
    let stdout = Buffer.contents job.stdout in
    let stderr = Buffer.contents job.stderr in
    lwt (stdout, stderr)
  in
  match !(job.process) with
  | Pending -> lwt {Endpoints.Job.Response.status = Pending; stdout = ""; stderr = ""}
  | Started process ->
    match process#state with
    | Running ->
      let%lwt (stdout, stderr) = read_outputs process in
      lwt {Endpoints.Job.Response.status = Running; stdout; stderr}
    | Exited status ->
      let%lwt (stdout, stderr) = read_outputs process in
      Log.debug (fun m -> m "Ran job %s:@\n%a" (JobId.to_string id) pp_command job.command);
      Log.debug (fun m -> m "Status: %a" Process.pp_process_status status);
      Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stdout") stdout);
      Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stderr") stderr);
      Lwt_list.iter_p
        (fun fname ->
          try%lwt
            Lwt_unix.unlink fname
          with
            | Unix.(Unix_error (ENOENT, _, _)) -> lwt_unit
        )
        job.files;%lwt
      lwt
        Endpoints.Job.Response.{status = if status = WEXITED 0 then Succeeded else Failed;
          stdout;
          stderr}

let jobs : (JobId.t, t) Hashtbl.t = Hashtbl.create 8
let (pending_jobs : t Lwt_stream.t), add_pending_job = Lwt_stream.create ()
let register_job job =
  Log.debug (fun m -> m "Register job %s:@\n%a" (JobId.to_string job.id) pp_command job.command);
  add_pending_job (Some job);
  Hashtbl.add jobs job.id job

let run_job job =
  match !(job.process) with
  | Started _ -> invalid_arg "run_job: cannot start a job that is already started"
  | Pending ->
    Log.debug (fun m -> m "Running job %s:@\n%a" (JobId.to_string job.id) pp_command job.command);
    let process = Lwt_process.open_process_full ("", job.command) in
    job.process := Started process;
    Lwt_io.close process#stdin;%lwt
    (* block until the job is done running *)
    ignore <$> process#status

let call_nix_build ?(files = []) expr =
  let id = JobId.create () in
  let command = [|
    "nix";
    "--extra-experimental-features";
    "nix-command";
    "build";
    "--print-build-logs";
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
  let stdout = Buffer.create 8 in
  let stderr = Buffer.create 8 in
  let job = {id; command; process; stdout; stderr; files} in
  register_job job;
  lwt job

let get id =
  match Hashtbl.find_opt jobs id with
  | None -> Madge_server.shortcut_not_found "This job does not exit anymore, or has never existed."
  | Some job -> lwt job

let status id = status id =<< get id

type output = {out: string} [@@deriving of_yojson]
type result = {outputs: output list} [@@deriving of_yojson]

let file id _slug =
  let%lwt response = status id in
  match response.status with
  | Pending -> Madge_server.shortcut_bad_request "This job is not running yet, you cannot query the file."
  | Running -> Madge_server.shortcut_bad_request "This job is still running, you cannot query the file."
  | Failed -> Madge_server.shortcut_bad_request "This job failed, you cannot query the file."
  | Succeeded ->
    match Yojson.Safe.from_string response.stdout with
    (* two cases depending on whether there is `startTime` and `endTime` or not *)
    | `List [`Assoc [_; ("outputs", `Assoc [("out", `String fname)]); _; _]]
    | `List [`Assoc [_; ("outputs", `Assoc [("out", `String fname)])]] ->
      Madge_server.respond_file ~fname
    | _ -> assert false

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Job.t -> a = fun _env endpoint ->
  match endpoint with
  | Status -> status
  | File -> file
