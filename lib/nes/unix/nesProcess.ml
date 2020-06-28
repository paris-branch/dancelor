open Nes

module Log = (val Logs.src_log (Logs.Src.create "nes.unix.process") : Logs.LOG)

type process_status = Unix.process_status =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int
[@@deriving show]

type output =
  { stdout : string ;
    stderr : string ;
    status : process_status }

let escape_shell_argument =
  String.split_on_char '\''
  ||> String.concat "'\\''"
  ||> fun s -> "'" ^ s ^ "'"

let command =
  List.map escape_shell_argument
  ||> String.concat " "

let chdir path cmd =
  (command ["cd"; path]) ^ " && " ^ cmd

let check_output
    ?(check_status_ok=false) ?(check_no_stderr=false) ?(check_no_stdout=false)
    ?(loglevel_on_ok=Logs.Debug) ?(loglevel_on_error=Logs.Debug)
    ?command
    out
  =
  let error =
    if check_status_ok && out.status <> Unix.WEXITED 0 then
      Some "did not exit successfully"
    else if check_no_stdout && out.stdout <> "" then
      Some "does not have an empty stdout"
    else if check_no_stderr && out.stderr <> "" then
      Some "does not have an empty stderr"
    else
      None
  in
  let loglevel =
    match error with
    | Some error ->
      Log.msg loglevel_on_error (fun m -> m "The command %s" error);
      (match command with
       | Some command -> Log.msg loglevel_on_error (fun m -> m "Command: %s" command);
       | None -> ());
      loglevel_on_error
    | _ -> loglevel_on_ok
  in
  Log.msg loglevel (fun m -> m "Status: %a" pp_process_status out.status);
  Log.msg loglevel (fun m -> m "%a" (Format.pp_multiline_sensible "Stdout") out.stdout);
  Log.msg loglevel (fun m -> m "%a" (Format.pp_multiline_sensible "Stderr") out.stderr);
  (
    match error with
    | Some error -> failwith ("NesProcess.run: " ^ error)
    | None -> ()
  )

let run
    ?timeout ?env ?cwd ?(stdin="")
    ?check_status_ok ?check_no_stdout ?check_no_stderr
    ?loglevel_on_ok ?loglevel_on_error
    cmd
  =
  let strcmd =
    let pre,post =
      match cwd with
        None -> "",""
      | Some cwd -> "cd " ^ escape_shell_argument cwd ^ " && { ", "; }"
    in
    pre ^ String.concat " " (List.map escape_shell_argument cmd) ^ post
  in
  Log.debug (fun m -> m "Command: %s" strcmd);
  let cmd = Lwt_process.shell strcmd in
  Lwt_process.with_process_full ?timeout ?env cmd @@ fun process ->
  Lwt_io.write process#stdin stdin; %lwt
  let%lwt status = process#status in
  let%lwt stdout = Lwt_io.read process#stdout in
  let%lwt stderr = Lwt_io.read process#stderr in
  let output = { status ; stdout ; stderr } in
  check_output
    ?check_status_ok ?check_no_stderr ?check_no_stdout
    ?loglevel_on_ok ?loglevel_on_error ~command:strcmd
    output;
  Lwt.return output

let run_ignore
    ?timeout ?env ?cwd ?stdin
    ?check_status_ok ?check_no_stdout ?check_no_stderr
    ?loglevel_on_ok ?loglevel_on_error
    cmd
  =
  let%lwt _ =
    run
      ?timeout ?env ?cwd ?stdin
      ?check_status_ok ?check_no_stdout ?check_no_stderr
      ?loglevel_on_ok ?loglevel_on_error
      cmd
  in
  Lwt.return_unit
