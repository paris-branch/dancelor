
let prefix = "db"

let read_json prefix dir file =
  let path = Filename.concat (Filename.concat prefix dir) file in
  let ichan = open_in path in
  let json = Ezjsonm.from_channel ichan in
  close_in ichan;
  json
