open ExtPervasives

let prefix = Config.database

let list_entries table =
  Filename.concat prefix table
  |> Sys.readdir
  |> Array.to_list

let read table entry file =
  Logs.debug (fun m -> m "Reading %s/%s/%s" table entry file);
  let path = ExtFilename.concat_l [prefix; table; entry; file] in
  let ichan = open_in path in
  let content = in_channel_to_string ichan in
  close_in ichan;
  content

let read_json table entry  =
  read table entry
  ||> Ezjsonm.from_string

let write table entry file content =
  let path = ExtFilename.concat_l [prefix; table; entry] in
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o777;
  let path = Filename.concat path file in
  let ochan = open_out path in
  output_string ochan content;
  close_out ochan

let write_json table entry file =
  JsonHelpers.check_object
  ||> Ezjsonm.to_string
  ||> write table entry file
