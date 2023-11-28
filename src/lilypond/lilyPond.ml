open Nes
module Log = (val Logs.(src_log (Src.create "lilypond")) : Logs.LOG)

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

(** Alias of {!run} for SVG generation. *)
let svg ?lilypond_bin ?exec_path ?(options=[]) filename =
  run ?lilypond_bin ?exec_path ~options:("-dbackend=svg" :: options) filename

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
