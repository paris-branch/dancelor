open Nes
include Madge

let cohttp_code_meth_to_meth = function
  | `GET -> GET
  | `POST -> POST
  | `HEAD -> HEAD
  | `DELETE -> DELETE
  | `PATCH -> PATCH
  | `PUT -> PUT
  | `OPTIONS -> OPTIONS
  | `TRACE -> TRACE
  | `CONNECT -> CONNECT
  | `Other _ ->
    assert false (* FIXME *)

let match_apply
  : type a r. (a, r Lwt.t, r) route ->
  a ->
  request ->
  (unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) option
= fun route controller request ->
  match_ route controller request @@ fun (module R) promise ->
  Lwt.bind promise @@ fun value ->
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
  let body = Yojson.Safe.to_string (R.to_yojson value) in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status: `OK ~body ()

exception Shortcut of (Cohttp.Response.t * Cohttp_lwt.Body.t)

let shortcut p : Void.t Lwt.t = Lwt.bind p @@ fun x -> raise (Shortcut x)
