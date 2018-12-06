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
