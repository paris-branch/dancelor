open Nes
module Log = (val Logs.(src_log (Src.create "lilypond")) : Logs.LOG)

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
        let%lwt out = NesProcess.run ["xvfb-run"; !bin; "--version"] in
        let b =
          if String.starts_with ~needle:"Inkscape 0." out.stdout then
            (
              Log.debug (fun m -> m "Inkscape's version is < 1");
              true
            )
          else if String.starts_with ~needle:"Inkscape 1." out.stdout then
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

  let crop ~exec_path ~output file =
    let%lwt cmd =
      if%lwt is_version_pre1 () then
        Lwt.return ["xvfb-run"; get_bin (); "--without-gui"; "--export-area-drawing"; "--export-plain-svg="^output; file]
      else
        Lwt.return ["xvfb-run"; get_bin (); "--export-area-drawing"; "--export-plain-svg"; "--export-filename="^output; file]
    in
    try%lwt
      NesProcess.run_ignore
        ~cwd:exec_path
        ~on_wrong_status:Logs.Error
        ~on_nonempty_stdout:Logs.Warning ~on_nonempty_stderr:Logs.Warning
        cmd
    with
      Failure _ -> Lwt.return_unit
end

let run ?(lilypond_bin="lilypond") ?(exec_path=".") ?(options=[]) filename =
  try%lwt
    NesProcess.run_ignore
      ~env:[|"PATH="^(Unix.getenv "PATH");
             "HOME="^(Unix.getenv "HOME");
             "LANG=en"|]
      ~cwd:exec_path
      ~on_wrong_status:Logs.Error
      ~on_nonempty_stdout:Logs.Warning ~on_nonempty_stderr:Logs.Warning
      ([lilypond_bin; "--loglevel=WARNING"; "-dno-point-and-click"] @ options @ [filename])
  with
    Failure _ -> Lwt.return_unit

let cropped_svg ?lilypond_bin ?(exec_path=".") filename =
  run ?lilypond_bin ~exec_path ~options:["-dbackend=svg"] filename;%lwt
  Inkscape.crop
    ~exec_path
    ((Filename.chop_extension filename) ^ ".svg")
    ~output:((Filename.chop_extension filename) ^ ".cropped.svg")

let ogg ?lilypond_bin ?(exec_path=".") filename =
  run ?lilypond_bin ~exec_path filename;%lwt
  try%lwt
    NesProcess.run_ignore ~cwd:exec_path
      ~on_wrong_status:Logs.Error
      ~on_nonempty_stdout:Logs.Error
      ~on_nonempty_stderr:Logs.Error
      [ "timidity"; "-Ov"; "--quiet=7";
        (Filename.chop_extension filename)^".midi" ]
  with
    Failure _ -> Lwt.return_unit
