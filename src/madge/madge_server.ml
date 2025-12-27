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
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
  let body = Yojson.Safe.to_string @@ `Assoc ["message", `String message] in
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

module type Endpoints = sig
  include Madge.Endpoints
  type env
  val dispatch : 'a 'r. env -> ('a, 'r Lwt.t, 'r) t -> 'a
  val namespace : string
end

module type Apply_controller = sig
  type env
  val apply_controller : env -> Request.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
end

module Make_apply_controller (E : Endpoints) : Apply_controller with type env = E.env = struct
  let api_request_duration_seconds =
    let family =
      Prometheus.DefaultHistogram.v_labels
        ~namespace: E.namespace
        ~help: "API request duration in seconds"
        "api_request_duration_seconds"
        ~label_names: ["endpoint"]
    in
    fun ~endpoint ->
      Prometheus.DefaultHistogram.labels family [endpoint]

  type env = E.env

  let apply_controller env request =
    let rec madge_match_apply_all = function
      | [] -> None
      | E.W endpoint :: wrapped_endpoints ->
        (
          match match_apply (E.route endpoint) (fun () -> E.dispatch env endpoint) request with
          | None -> madge_match_apply_all wrapped_endpoints
          | Some f -> Some (E.name endpoint, f)
        )
    in
    (* FIXME: We should just get a URI. *)
    match madge_match_apply_all E.all with
    | None -> respond_not_found "The endpoint `%s` does not exist or is not called with the right method and parameters." (Uri.path @@ Madge.Request.uri request)
    | Some (name, thunk) ->
      Prometheus.DefaultHistogram.time
        (api_request_duration_seconds ~endpoint: name)
        Unix.gettimeofday
        thunk

  let apply_controller env request =
    let process_batched_requests =
      Lwt_list.map_p (fun request ->
        let%lwt (response, body) = apply_controller env request in
        let%lwt body = Madge.Response.body_of_lwt body in
        lwt (response, body)
      )
    in
    match match_apply E.(route_full Batch) (fun () -> process_batched_requests) request with
    | Some thunk -> thunk ()
    | None -> apply_controller env request
end
