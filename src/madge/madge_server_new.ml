open Nes
include Madge

(* FIXME: This is not just server, but a very specific type of server. Either
   make it Dancelor-specific, or rename in eg. Madge_yojson_server. *)

let match_apply
  : type a r. (a, r Lwt.t, r) route ->
    a ->
    Uri.t ->
    string Lwt.t option
  = fun route controller uri ->
    match_ route controller uri @@ fun (module R) thunk ->
    Lwt.map (Yojson.Safe.to_string % R.to_yojson) (thunk ())
