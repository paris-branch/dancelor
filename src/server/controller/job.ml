open NesUnix
open Common

module Log = (val Logger.create "controller.job": Logs.LOG)

type command = string array [@@deriving show]

type t = {
  command: command;
  process: Lwt_process.process_full;
  stdout: Buffer.t;
  stderr: Buffer.t;
}

let rec read_in_channel_into_buffer ichan buf =
  let%lwt chunk = Lwt_io.read ~count: 1024 ichan in
  Buffer.add_string buf chunk;
  match String.length chunk with
  | 1024 -> read_in_channel_into_buffer ichan buf
  | _ -> lwt_unit

let status id job =
  let read_outputs () =
    read_in_channel_into_buffer job.process#stdout job.stdout;%lwt
    read_in_channel_into_buffer job.process#stderr job.stderr;%lwt
    let stdout = Buffer.contents job.stdout in
    let stderr = Buffer.contents job.stderr in
    lwt (stdout, stderr)
  in
  match job.process#state with
  | Running ->
    let%lwt (stdout, stderr) = read_outputs () in
    lwt {Endpoints.Job.Response.status = Running; stdout; stderr}
  | Exited status ->
    let%lwt (stdout, stderr) = read_outputs () in
    Log.debug (fun m -> m "Ran job %s:@\n%a" (JobId.to_string id) pp_command job.command);
    Log.debug (fun m -> m "Status: %a" Process.pp_process_status status);
    Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stdout") stdout);
    Log.debug (fun m -> m "%a" (Format.pp_multiline_sensible "Stderr") stderr);
    lwt {
      Endpoints.Job.Response.status = if status = WEXITED 0 then Succeeded else Failed;
      stdout;
      stderr
    }

let jobs : (JobId.t, t) Hashtbl.t = Hashtbl.create 8

let call_nix_build expr =
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
  Log.debug (fun m -> m "Running job %s:@\n%a" (JobId.to_string id) pp_command command);
  let process = Lwt_process.open_process_full ("", command) in
  Lwt_io.close process#stdin;%lwt
  let stdout = Buffer.create 8 in
  let stderr = Buffer.create 8 in
  Hashtbl.add jobs id {command; process; stdout; stderr};
  lwt id

(** For use in {!Routine}. *)
let wait_ignore id =
  match Hashtbl.find_opt jobs id with
  | None -> lwt_unit
  | Some job -> ignore <$> job.process#status

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
