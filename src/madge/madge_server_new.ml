open Nes
include Madge

(* FIXME: This is not just server, but a very specific type of server. Either
   make it Dancelor-specific, or rename in eg. Madge_yojson_cohttp_server. *)

let match_apply
  : type a r. (a, r Lwt.t, r) route ->
    a ->
    Uri.t ->
    (unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) option
  = fun route controller uri ->
    match_ route controller uri @@ fun (module R) promise ->
    Lwt.bind promise @@ fun value ->
    let body = Yojson.Safe.to_string (R.to_yojson value) in
    Cohttp_lwt_unix.Server.respond_string ~status: `OK ~body ()

exception Shortcut of (Cohttp.Response.t * Cohttp_lwt.Body.t)

let shortcut p : Void.t Lwt.t = Lwt.bind p @@ fun x -> raise (Shortcut x)
