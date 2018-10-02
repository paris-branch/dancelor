open Dancelor_common

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

let read_json table entry file =
  let path = Filename.concat_l [prefix; table; entry; file] in
  let ichan = open_in path in
  let json = Ezjsonm.from_channel ichan in
  close_in ichan;
  json
