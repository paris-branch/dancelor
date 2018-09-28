open Dancelor_common
   
let prefix = "db"

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
