open ExtPervasives
module Log = (val Log.create "dancelor.common.config" : Logs.LOG)

let config =
  Log.debug (fun m -> m "Loading configuration");
  try
    let ichan = open_in Sys.argv.(1) in
    let config =
      in_channel_to_string ichan
      |> Ezjsonm.from_string
    in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully:@\n%s" (Ezjsonm.to_string config));
    config
  with
    exn ->
    Log.debug (fun m -> m "No configuration found:@\n%s" (Printexc.to_string exn));
    `Null (* FIXME *)

let read_config ~type_ ~default path =
  try type_ (Ezjsonm.find config path)
  with Not_found -> default

let int = Ezjsonm.get_int
let string = Ezjsonm.get_string

let port = read_config ~type_:int ~default:8080 ["port"]
let cache = read_config ~type_:string ~default:"cache" ["cache"]
let database = read_config ~type_:string ~default:"database" ["database"]
let share = read_config ~type_:string ~default:"share" ["share"]
let lilypond = read_config ~type_:string ~default:"lilypond" ["lilypond"]
