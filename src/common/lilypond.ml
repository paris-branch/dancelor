open ExtPervasives

let escape_shell_argument =
  String.split_on_char '\''
  ||> String.concat "'\\''"
  ||> fun s -> "'" ^ s ^ "'"

let shell_cmdline =
  List.map escape_shell_argument
  ||> String.concat " "

let channel_get_contents ic =
  let bufsize = 1024 in
  let buf = Bytes.create bufsize in
  let output = Buffer.create bufsize in
  let len = ref (-1) in
  while !len <> 0 do
    len := input ic buf 0 bufsize;
    Buffer.add_subbytes output buf 0 !len
  done;
  Buffer.contents output

(* FIXME: there are lots of options in 'lilypond -dhelp' *)

let lilypond options filename =
  let cmdline =
    options
    |> List.map (fun (key, value) -> Format.sprintf "--%s=%s" key value)
    |> (fun options -> "lilypond" :: options @ [filename])
    |> shell_cmdline
  in
  let env =
    [| "LANG=en" |]
  in
  let stdout, stdin, errout =
    Unix.open_process_full cmdline env
  in
  close_out stdin;
  assert (channel_get_contents stdout = "");
  let _errout_string = channel_get_contents errout in
  let _status = Unix.close_process_full (stdout, stdin, errout) in
  assert false
