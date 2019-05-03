open Nes open NesOption

let int = Json.int
let string = Json.string

let bool =
  Json.string >=> function
  | "true" | "yes" | "on" -> Some true
  | "false" | "no" | "off" -> Some false
  | _ -> None

let loglevel_of_string json =
  match Json.string json with
  | None -> None
  | Some "error" -> Some Logs.Error
  | Some "warning" -> Some Logs.Warning
  | Some "info" -> Some Logs.Info
  | Some "debug" -> Some Logs.Debug
  | _ -> assert false

(* =========================== [ Dynamic Stuff ] ============================ *)

let port = ref 8080
let cache = ref "cache"
let database = ref "database"
let share = ref "share"
let lilypond = ref "lilypond"
let loglevel = ref Logs.Debug
let routines = ref true

let load_from_file filename =
  let config =
    try
      let ichan = open_in filename in
      let config =
        in_channel_to_string ichan
        |> Json.from_string
      in
      close_in ichan;
      config
    with
      exn -> raise exn
  in
  let field config ~type_ ~default path =
    match Json.(get_opt ~k:type_ path config) with
    | None -> default
    | Some value -> value
  in
  port := field config ~type_:int ~default:8080 ["port"];
  cache := field config  ~type_:string ~default:"cache" ["cache"];
  database := field config ~type_:string ~default:"database" ["database"];
  share := field config ~type_:string ~default:"share" ["share"];
  lilypond := field config ~type_:string ~default:"lilypond" ["lilypond"];
  loglevel := field config ~type_:loglevel_of_string ~default:Logs.Debug ["loglevel"];
  routines := field config ~type_:bool ~default:true ["routines"]
