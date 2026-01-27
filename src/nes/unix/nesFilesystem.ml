open NesPervasives

let read_file file =
  Lwt_io.with_file ~mode: Input file @@ fun ichan ->
  Lwt_io.read ichan

let write_file file content =
  Lwt_io.with_file ~mode: Output file @@ fun ochan ->
  Lwt_io.write ochan content

let remove_file file =
  Sys.remove file

let create_directory ?(fail_if_exists = true) path =
  if%lwt not <$> Lwt_unix.file_exists path then
    Lwt_unix.mkdir path 0o777
  else if fail_if_exists then
    failwith "NesFilesystem.create_directory"
  else lwt_unit

let read_directory path =
  Sys.readdir path |> Array.to_list

let remove_directory path =
  Lwt_unix.rmdir path
