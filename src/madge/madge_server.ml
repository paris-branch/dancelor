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
    let headers = Cohttp.Header.of_list [("Content-Type", "application/x-biniou")] in
    let body = Bi_io.string_of_tree @@ R.to_biniou value in
    Cohttp_lwt_unix.Server.respond_string ~status: `OK ~headers ~body ()
  with
    | Shortcut response -> Lwt.return response

let shortcut p : 'any Lwt.t = Lwt.bind p @@ fun x -> raise (Shortcut x)

let respond_string ?content_type body =
  let headers = Cohttp.Header.of_list (List.map (Pair.cons "Content-Type") @@ Option.to_list content_type) in
  shortcut @@ Cohttp_lwt_unix.Server.respond_string ~status: `OK ~headers ~body ()

let respond_file ~fname =
  let content_type =
    match Filename.extension fname with
    | ".pdf" -> Some "application/pdf"
    | ".svg" -> Some "image/svg+xml"
    | ".ogg" -> Some "audio/ogg"
    | ".webp" -> Some "image/webp"
    | _ -> None
  in
  let headers = Cohttp.Header.of_list (List.map (Pair.cons "Content-Type") @@ Option.to_list content_type) in
  shortcut @@ Cohttp_lwt_unix.Server.respond_file ~headers ~fname ()

let respond status =
  Format.kasprintf @@ fun message ->
  let headers = Cohttp.Header.of_list [("Content-Type", "application/x-biniou")] in
  let body = Bi_io.string_of_tree @@ `Record [|Some "message", Bi_io.hash_name "message", `String message|] in
  Cohttp_lwt_unix.Server.respond_string ~status ~headers ~body ()

let respond_not_found fmt = respond `Not_found fmt
let respond_internal_server_error () = respond `Internal_server_error "Internal server error."

let shortcut' status =
  Format.kasprintf @@ fun message ->
  shortcut @@ respond status "%s" message

let shortcut_not_found message = shortcut' `Not_found message
let shortcut_forbidden message = shortcut' `Forbidden message
let shortcut_forbidden_no_leak () = shortcut_forbidden "Forbidden."
let shortcut_bad_request message = shortcut' `Bad_request message
