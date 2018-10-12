open ExtPervasives

let prefix =
  let env_var = "DANCELOR_DATABASE" in
  try
    Sys.getenv env_var
  with
    Not_found ->
     Format.eprintf "The environment variable %s is required.@." env_var;
     exit 1

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
