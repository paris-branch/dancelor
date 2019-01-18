open ExtPervasives

let config =
  try
    let ichan = open_in Sys.argv.(1) in
    let config =
      in_channel_to_string ichan
      |> Json.from_string
    in
    close_in ichan;
    config
  with
    exn -> raise exn

let read_config ~type_ ~default path =
  match Json.(get_opt ~k:type_ path config) with
  | None -> default
  | Some value -> value

let int = Json.int
let string = Json.string

let loglevel json =
  match Json.string json with
  | None -> None
  | Some "error" -> Some Logs.Error
  | Some "warning" -> Some Logs.Warning
  | Some "info" -> Some Logs.Info
  | Some "debug" -> Some Logs.Debug
  | _ -> assert false

(* =========================== [ Dynamic Stuff ] ============================ *)

let port = read_config ~type_:int ~default:8080 ["port"]
let cache = read_config ~type_:string ~default:"cache" ["cache"]
let database = read_config ~type_:string ~default:"database" ["database"]
let share = read_config ~type_:string ~default:"share" ["share"]
let lilypond = read_config ~type_:string ~default:"lilypond" ["lilypond"]
let loglevel = read_config ~type_:loglevel ~default:Logs.Debug ["loglevel"]

(* ============================ [ Static Shit ] ============================= *)

let api_prefix = "/api"
