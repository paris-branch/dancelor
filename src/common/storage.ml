open ExtPervasives

let prefix = Config.database_prefix ()

let list_entries table =
  Filename.concat prefix table
  |> Sys.readdir
  |> Array.to_list

let read table entry file =
  let path = ExtFilename.concat_l [prefix; table; entry; file] in
  let ichan = open_in path in
  let content = in_channel_to_string ichan in
  close_in ichan;
  content

let read_json table entry file =
  read table entry file
  |> Ezjsonm.from_string

let read_yaml table entry file =
  read table entry file
  |> Yaml.of_string_exn

let write table entry file content =
  let path = ExtFilename.concat_l [prefix; table; entry] in
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o777;
  let path = Filename.concat path file in
  let ochan = open_out path in
  output_string ochan content;
  close_out ochan

let write_json table entry file json =
  Ezjsonm.to_string json
  |> write table entry file

let write_yaml table entry file yaml =
  Yaml.to_string_exn yaml
  |> write table entry file
