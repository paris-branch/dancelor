open Nes
include Madge

type response = {
  status: Cohttp.Code.status_code;
  headers: Cohttp.Header.t;
  body: Yojson.Safe.t;
}

let match_apply
  : type a r. (a, r Lwt.t, r) Route.t ->
  (unit -> a) ->
  Request.t ->
  (unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) option
= fun route controller request ->
  apply route controller request @@ fun (module R) promise ->
  Lwt.bind promise @@ fun value ->
  let status = `OK in
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
  let body = Yojson.Safe.to_string @@ R.to_yojson value in
  Cohttp_lwt_unix.Server.respond_string ~status ~headers ~body ()

exception Shortcut of (Cohttp.Response.t * Cohttp_lwt.Body.t)

let shortcut p : 'any Lwt.t = Lwt.bind p @@ fun x -> raise (Shortcut x)

let shortcut' ?msg status =
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
  let body =
    match msg with
    | None -> "{}"
    | Some msg -> Yojson.Safe.to_string @@ `Assoc ["message", `String msg]
  in
  shortcut @@ Cohttp_lwt_unix.Server.respond_string ~status ~headers ~body ()
