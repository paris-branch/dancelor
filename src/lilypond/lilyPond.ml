open Nes
module Log = (val Logs.(src_log (Src.create "lilypond")) : Logs.LOG)

module Inkscape = struct
  let crop ?(inkscape_bin="inkscape") ~exec_path ~output file =
    let cmd = [
      "xvfb-run"; inkscape_bin;
      "--export-area-drawing"; "--export-plain-svg"; "--export-filename="^output; file
    ]
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

let cropped_svg ?lilypond_bin ?inkscape_bin ?(exec_path=".") filename =
  run ?lilypond_bin ~exec_path ~options:["-dbackend=svg"] filename;%lwt
  Inkscape.crop ?inkscape_bin ~exec_path
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
