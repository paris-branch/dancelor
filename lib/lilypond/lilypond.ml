open Nes
module Log = (val Logs.(src_log (Src.create "lilypond")) : Logs.LOG)

let escape_shell_argument =
  String.split_on_char '\''
  ||> String.concat "'\\''"
  ||> fun s -> "'" ^ s ^ "'"

let shell_cmdline s =
  List.map escape_shell_argument s
  |> String.concat " "

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
