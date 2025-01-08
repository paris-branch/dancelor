open Nes
include Madge_common_new

let match_apply
  : type a r. (a, r Lwt.t, r) Route.t ->
    a ->
    Uri.t ->
    string Lwt.t option
  = fun route controller uri ->
    Route.match_ route controller uri @@ fun (module R) thunk ->
    Lwt.map (Yojson.Safe.to_string % R.to_yojson) (thunk ())
