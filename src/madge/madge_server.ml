open Nes
include Madge

(** Exception to shortcut execution and return something else immediately. *)
exception Shortcut of (Cohttp.Response.t * Cohttp_lwt.Body.t)

let match_apply
  : type a r. (a, r Lwt.t, r) Route.t ->
  (unit -> a) ->
  Request.t ->
  (unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t) option
= fun route controller request ->
  apply route controller request @@ fun (module R) f ->
  try%lwt
    let%lwt value = f () in
    let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
    let body = Yojson.Safe.to_string @@ R.to_yojson value in
    Cohttp_lwt_unix.Server.respond_string ~status: `OK ~headers ~body ()
  with
    | Shortcut response -> Lwt.return response

let shortcut p : 'any Lwt.t = Lwt.bind p @@ fun x -> raise (Shortcut x)

let respond_string ~content_type body =
  let headers = Cohttp.Header.of_list [("Content-Type", content_type)] in
  shortcut @@ Cohttp_lwt_unix.Server.respond_string ~status: `OK ~headers ~body ()

let respond_file ~content_type ~fname =
  let headers = Cohttp.Header.of_list [("Content-Type", content_type)] in
  shortcut @@ Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()

let shortcut' ~msg status =
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
  let body = Yojson.Safe.to_string @@ `Assoc ["message", `String msg] in
  shortcut @@ Cohttp_lwt_unix.Server.respond_string ~status ~headers ~body ()

let respond_not_found msg = shortcut' ~msg `Not_found
let respond_forbidden msg = shortcut' ~msg `Forbidden
let respond_forbidden_no_leak () = respond_forbidden "Forbidden."
let respond_bad_request msg = shortcut' ~msg `Bad_request
