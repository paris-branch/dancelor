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

type response = {
  status: Cohttp.Code.status_code;
  headers: Cohttp.Header.t;
  body: Yojson.Safe.t;
}

let match_apply
  : type a r. ?post_process: (response -> response) ->
  (a, r Lwt.t, r) route ->
  a ->
  request ->
  (unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) option
= fun ?(post_process = Fun.id) route controller request ->
  match_ route controller request @@ fun (module R) promise ->
  Lwt.bind promise @@ fun value ->
  let response = {
    status = `OK;
    headers = Cohttp.Header.of_list [("Content-Type", "application/json")];
    body = R.to_yojson value;
  }
  in
  let {status; headers; body} = post_process response in
  let body = Yojson.Safe.to_string body in
  Cohttp_lwt_unix.Server.respond_string ~status ~headers ~body ()

exception Shortcut of (Cohttp.Response.t * Cohttp_lwt.Body.t)

let shortcut p : Void.t Lwt.t = Lwt.bind p @@ fun x -> raise (Shortcut x)
