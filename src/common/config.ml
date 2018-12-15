open ExtPervasives
module Log = (val Log.create "dancelor.common.config" : Logs.LOG)

let config =
  Log.debug (fun m -> m "Loading configuration");
  try
    let ichan = open_in Sys.argv.(1) in
    let config =
      in_channel_to_string ichan
      |> Json.from_string
    in
    close_in ichan;
    Log.debug (fun m -> m "Loaded successfully:@\n%s" (Json.to_string config));
    config
  with
    exn ->
    Log.debug (fun m -> m "No configuration found:@\n%s" (Printexc.to_string exn));
    raise exn

let read_config ~type_ ~default path =
  try Json.(get ~k:type_ path config)
  with Not_found -> default

let int = Json.int
let string = Json.string

(* =========================== [ Dynamic Stuff ] ============================ *)

let port = read_config ~type_:int ~default:8080 ["port"]
let cache = read_config ~type_:string ~default:"cache" ["cache"]
let database = read_config ~type_:string ~default:"database" ["database"]
let share = read_config ~type_:string ~default:"share" ["share"]
let lilypond = read_config ~type_:string ~default:"lilypond" ["lilypond"]

(* ============================ [ Static Shit ] ============================= *)

let api_prefix = "/api"
