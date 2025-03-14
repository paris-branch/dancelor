open NesPervasives

let read_file file =
  let ichan = open_in file in
  let content = in_channel_to_string ichan in
  close_in ichan;
  content

let write_file file content =
  let ochan = open_out file in
  output_string ochan content;
  close_out ochan

let remove_file file =
  Sys.remove file

let create_directory ?(fail_if_exists = true) path =
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o777
  else if fail_if_exists then
    failwith "NesFilesystem.create_directory"

let read_directory path =
  Sys.readdir path |> Array.to_list

let remove_directory path =
  Unix.rmdir path
