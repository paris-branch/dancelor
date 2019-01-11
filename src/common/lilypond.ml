open ExtPervasives
module Log = (val Log.create "dancelor.common.lilypond" : Logs.LOG)

let escape_shell_argument =
  String.split_on_char '\''
  ||> String.concat "'\\''"
  ||> fun s -> "'" ^ s ^ "'"

let shell_cmdline s =
  List.map escape_shell_argument s
  |> String.concat " "

let run ?(exec_path=".") ?(options=[]) filename =
  let (>>=) = Lwt.bind in
  let open Lwt_process in

  with_process_full
    ~env:[|"PATH="^(Unix.getenv "PATH");
           "LANG=en"|]

    (shell
       ("cd " ^ (escape_shell_argument exec_path) ^ " && "
        ^ (shell_cmdline
             ([Config.lilypond; "--loglevel=WARNING"] @ options @ [filename]))))

    (fun process ->
      process#status >>= fun status ->
      (match status with
       | WEXITED 0 ->
          Lwt.return ()
       | _ ->
          Lwt_io.read process#stderr >>= fun output ->
          Log.err (fun m -> m "Error while running Lilypond:@\n%a" pp_string_multiline output);
          Lwt.return ())
    )
