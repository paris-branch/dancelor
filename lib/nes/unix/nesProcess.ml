open Nes

module Log = (val Logs.src_log (Logs.Src.create "nes.unix.process"): Logs.LOG)

type process_status = Unix.process_status =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int
[@@deriving show]

type output = {
  stdout: string;
  stderr: string;
  status: process_status;
}

let escape_shell_argument =
  String.split_on_char '\''
  ||> String.concat "'\\''"
  ||> fun s -> "'" ^ s ^ "'"

let command =
  List.map escape_shell_argument
  ||> String.concat " "

let chdir path cmd =
  (command ["cd"; path]) ^ " && " ^ cmd

type loglevel = Logs.level =
  | App
  | Error
  | Warning
  | Info
  | Debug

let compare_loglevel l1 l2 =
  if l1 = l2 then
    0
  else
    match l1, l2 with
    | Debug, _ -> 1
    | _, Debug -> -1
    | Info, _ -> 1
    | _, Info -> -1
    | Warning, _ -> 1
    | _, Warning -> -1
    | Error, _ -> 1
    | _, Error -> -1
    (* | App    , _ -> 1 | _, App     -> -1 *)
    | _ -> assert false

let check_output
  ?on_wrong_status
  ?on_nonempty_stdout
  ?on_nonempty_stderr
  ?(on_ok = Logs.Debug)
  ?command
  out =
  let checks =
    [
      out.status = Unix.WEXITED 0, on_wrong_status, "did not exit successfully";
      out.stdout = "", on_nonempty_stdout, "does not have an empty stdout";
      out.stderr = "", on_nonempty_stderr, "does not have an empty stderr";
    ]
  in
  (* Keep only the checks that went wrong. *)
  let checks =
    List.map_filter
      (fun (ok, on_error, msg) ->
        match on_error with
        | Some loglevel when not ok -> Some (loglevel, msg)
        | _ -> None)
      checks
  in
  (* Log all things that went wrong *)
  List.iter
    (fun (loglevel, msg) ->
      Log.msg loglevel (fun m -> m "The command %s" msg))
    checks;
  (* Find the highest loglevel among the things that went wrong. *)
  let loglevel =
    checks
    |> List.map fst
    |> List.cons on_ok
    |> List.sort compare_loglevel
    |> List.rev
    |> List.ft
  in
  (* Log everything with this loglevel now. *)
  (
  match command with
  | Some command -> Log.msg loglevel (fun m -> m "Command: %s" command)
  | None -> ());
  Log.msg loglevel (fun m -> m "Status: %a" pp_process_status out.status);
  Log.msg loglevel (fun m -> m "%a" (Format.pp_multiline_sensible "Stdout") out.stdout);
  Log.msg loglevel (fun m -> m "%a" (Format.pp_multiline_sensible "Stderr") out.stderr);
  (* If at least one check failed, fail. *)
  (
  match checks with
  | [] -> ()
  | _ -> failwith "NesProcess.run")

let run
  ?timeout
  ?env
  ?cwd
  ?(stdin = "")
  ?on_wrong_status
  ?on_nonempty_stdout
  ?on_nonempty_stderr
  ?on_ok
  cmd =
  let strcmd =
    let pre, post =
      match cwd with
      None -> "", ""
      | Some cwd -> "cd " ^ escape_shell_argument cwd ^ " && { ", "; }"
    in
    pre ^ String.concat " " (List.map escape_shell_argument cmd) ^ post
  in
  Log.debug (fun m -> m "Command: %s" strcmd);
  let cmd = Lwt_process.shell strcmd in
  Lwt_process.with_process_full ?timeout ?env cmd
  @@ fun process ->
    Lwt_io.write process#stdin stdin;%lwt
    let%lwt status = process#status in
    let%lwt stdout = Lwt_io.read process#stdout in
    let%lwt stderr = Lwt_io.read process#stderr in
    let output = { status; stdout; stderr } in
    check_output
      ?on_wrong_status
      ?on_nonempty_stdout
      ?on_nonempty_stderr
      ?on_ok
      ~command: strcmd
      output;
    Lwt.return output

let run_ignore
  ?timeout
  ?env
  ?cwd
  ?stdin
  ?on_wrong_status
  ?on_nonempty_stdout
  ?on_nonempty_stderr
  ?on_ok
  cmd =
  let%lwt _ =
    run
      ?timeout
      ?env
      ?cwd
      ?stdin
      ?on_wrong_status
      ?on_nonempty_stdout
      ?on_nonempty_stderr
      ?on_ok
      cmd
  in
  Lwt.return_unit
