open Nes

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
    ?(check_status_ok=false) ?(check_no_stderr=false)
    ?(check_no_stdout=false) out
  =
  (if check_status_ok && out.status <> Unix.WEXITED 0 then
     failwith "NesProcess.run: did not exit successfully");
  (if check_no_stdout && out.stdout <> "" then
     failwith "NesProcess.run: stdout non empty");
  (if check_no_stderr && out.stderr <> "" then
     failwith "NesProcess.run: stderr non empty")

let run
    ?timeout ?env ?cwd ?(stdin="")
    ?check_status_ok ?check_no_stderr ?check_no_stdout
    cmd
  =
  let cmd =
    let pre,post =
      match cwd with
        None -> "",""
      | Some cwd -> "cd " ^ escape_shell_argument cwd ^ " && { ", "; }"
    in
    pre ^ String.concat " " (List.map escape_shell_argument cmd) ^ post
  in
  let cmd = Lwt_process.shell cmd in
  Lwt_process.with_process_full ?timeout ?env cmd @@ fun process ->
  let%lwt () = Lwt_io.write process#stdin stdin in
  let%lwt status = process#status in
  let%lwt stdout = Lwt_io.read process#stdout in
  let%lwt stderr = Lwt_io.read process#stderr in
  let output = { status ; stdout ; stderr } in
  check_output ?check_status_ok ?check_no_stderr ?check_no_stdout output;
  Lwt.return output

let run_ignore
    ?timeout ?env ?cwd ?stdin
    ?check_status_ok ?check_no_stdout ?check_no_stderr
    cmd
  =
  let%lwt _ =
    run
      ?timeout ?env ?cwd ?stdin
      ?check_status_ok ?check_no_stdout ?check_no_stderr
      cmd
  in
  Lwt.return_unit
