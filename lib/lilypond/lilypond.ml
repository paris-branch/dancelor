open Nes
module Log = (val Logs.(src_log (Src.create "lilypond")) : Logs.LOG)

let escape_shell_argument =
  String.split_on_char '\''
  ||> String.concat "'\\''"
  ||> fun s -> "'" ^ s ^ "'"

let shell_cmdline s =
  List.map escape_shell_argument s
  |> String.concat " "

let lwt_process_pread2 ?timeout ?env ?cwd command =
  Lwt_process.with_process_full ?timeout ?env ?cwd command @@ fun process ->
  let%lwt status = process#status in
  let%lwt stdout = Lwt_io.read process#stdout in
  let%lwt stderr = Lwt_io.read process#stderr in
  Lwt.return (status, stdout, stderr)

module Inkscape = struct

  (* FIXME: when versions <1 become obsolete (out of Debian stable), remove the
     handling of different version numbers, and use the --export-overwrite
     option. *)

  let get_bin, is_version_pre1, set_bin =
    let bin = ref "inkscape" in
    let version_pre1 = ref None in
    let get_bin () =
      !bin
    in
    let is_version_pre1 () =
      match !version_pre1 with
      | None ->
        Log.debug (fun m -> m "Polling Inkscape's version");
        let%lwt (_, stdout, _) = lwt_process_pread2 ("", [|!bin; "--version"|]) in
        let b =
          if String.starts_with ~needle:"Inkscape 0." stdout then
            (
              Log.debug (fun m -> m "Inkscape's version is < 1");
              true
            )
          else if String.starts_with ~needle:"Inkscape 1." stdout then
            (
              Log.debug (fun m -> m "Inkscape's version is >= 1");
              false
            )
          else
            failwith "Inkscape.is_version_pre1"
        in
        version_pre1 := Some b;
        Lwt.return b
      | Some b -> Lwt.return b
    in
    let set_bin nbin =
      bin := nbin;
      version_pre1 := None
    in
    (get_bin, is_version_pre1, set_bin)

  let run ~exec_path cmd =
    let cmdline = shell_cmdline cmd in
    Log.debug (fun m -> m "Running %s" cmdline);
    let cmd = Lwt_process.shell ("cd " ^ (escape_shell_argument exec_path) ^ " && " ^ cmdline) in
    match%lwt lwt_process_pread2 cmd with
    | (Unix.WEXITED 0, _, _) ->
      Lwt.return ()

    | (_, stdout, stderr) ->
      Log.warn (fun m -> m "Inkscape failed!");
      Log.warn (fun m -> m "Command line was:@\n%s" cmdline);

      (* FIXME: log error status *)

      if stdout = ""
      then Log.warn (fun m -> m "Standard output is empty")
      else Log.warn (fun m -> m "Standard output:@\n%s" stdout);

      if stderr = ""
      then Log.warn (fun m -> m "Standard error is empty")
      else Log.warn (fun m -> m "Standard error:@\n%s" stderr);

      Lwt.fail (Failure "Inkscape")

  let crop ~exec_path ~output file =
    let%lwt cmd =
      if%lwt is_version_pre1 () then
        Lwt.return [get_bin (); "--without-gui"; "--export-area-drawing"; "--export-plain-svg="^output; file]
      else
        Lwt.return [get_bin (); "--export-area-drawing"; "--export-plain-svg"; "--export-filename="^output; file]
    in
    run ~exec_path cmd
end

let run ?(lilypond_bin="lilypond") ?(exec_path=".") ?(options=[]) filename =
  let (>>=) = Lwt.bind in
  let open Lwt_process in

  Log.debug (fun m -> m "Running Lilypond[%s]..." filename);

  Lwt.catch

    (fun () ->
      with_process_full
        ~env:[|"PATH="^(Unix.getenv "PATH");
               "LANG=en"|]

        (shell
           ("cd " ^ (escape_shell_argument exec_path) ^ " && "
            ^ (shell_cmdline
                 ([lilypond_bin; "--loglevel=WARNING"; "-dno-point-and-click"] @ options @ [filename]))))

        (fun process ->
          process#status >>= fun status ->
          (match status with
           | WEXITED 0 ->
              Log.debug (fun m -> m "Lilypond[%s] exited with success." filename);
              Lwt.return ()
           | _ ->
              Log.debug (fun m -> m "About to read Lilypond[%s]'s stderr" filename);
              Lwt_io.read process#stderr >>= fun output ->
              Log.err (fun m -> m "Error while running Lilypond[%s]:@\n%a" filename pp_string_multiline output);
              Lwt.return ())))

    (function
     | Unix.Unix_error(Unix.EPIPE, _, _) ->
        Log.warn (fun m -> m "Lilypond[%s] triggered an EPIPE error" filename);
        Lwt.return ()
     | exn -> Lwt.fail exn)

let cropped_svg ?lilypond_bin ?(exec_path=".") filename =
  let%lwt () = run ?lilypond_bin ~exec_path ~options:["-dbackend=svg"] filename in
  Inkscape.crop
    ~exec_path
    ((Filename.chop_extension filename) ^ ".svg")
    ~output:((Filename.chop_extension filename) ^ ".cropped.svg")
