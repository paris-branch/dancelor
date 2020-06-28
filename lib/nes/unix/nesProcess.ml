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
    ?(check_status_ok=false) ?(check_no_stderr=false)
    ?(check_no_stdout=false) out
  =
  (if check_status_ok && out.status <> Unix.WEXITED 0 then
     failwith "NesProcess.run: did not exit successfully");
  (if check_no_stdout && out.stdout <> "" then
     failwith "NesProcess.run: stdout non empty");
  (if check_no_stderr && out.stderr <> "" then
     failwith "NesProcess.run: stderr non empty")

let pp_stdout_errout name fmt s =
  let rec remove_empty_last_lines = function
    | [] -> []
    | "" :: lines ->
      (
        match remove_empty_last_lines lines with
        | [] -> []
        | lines -> "" :: lines
      )
    | line :: lines ->
      line :: remove_empty_last_lines lines
  in
  s
  |> String.split_on_char '\n'
  |> remove_empty_last_lines
  |> (function [] -> ["(empty)"] | s -> s)
  |> (function
      | [] -> assert false
      | [line] -> fpf fmt "%s: %s" name line
      | lines ->
        fpf fmt "%s:" name;
        List.iter (fpf fmt "@\n%s") lines
    )

let run
    ?timeout ?env ?cwd ?(stdin="")
    ?check_status_ok ?check_no_stdout ?check_no_stderr
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
  Log.debug (fun m -> m "Command: %s" cmd);
  let cmd = Lwt_process.shell cmd in
  Lwt_process.with_process_full ?timeout ?env cmd @@ fun process ->
  Lwt_io.write process#stdin stdin; %lwt
  let%lwt status = process#status in
  Log.debug (fun m -> m "Status: %a" pp_process_status status);
  let%lwt stdout = Lwt_io.read process#stdout in
  Log.debug (fun m -> m "%a" (pp_stdout_errout "Stdout") stdout);
  let%lwt stderr = Lwt_io.read process#stderr in
  Log.debug (fun m -> m "%a" (pp_stdout_errout "Stderr") stderr);
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
