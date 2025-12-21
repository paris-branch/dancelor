open Nes
include Madge

type error =
  | Http of {request: Request.t; status: Cohttp.Code.status_code; message: string}
  | ServerUnreachable of {request: Request.t; status: Cohttp.Code.status_code}
  | BodyUnserialisation of {body: string; message: string}

exception Error of error

type error_response = {message: string} [@@deriving of_biniou, yojson]

let max_attempts = 10 (* up to ~2 minutes *)

let call_retry ?(retry = true) (request : Request.t) : (Response.t, error) result Lwt.t =
  let meth = Request.meth_to_cohttp_code_meth (Request.meth request) in
  let body = Cohttp_lwt.Body.of_string (Request.body request) in
  let rec call_retry attempt =
    let%lwt (response, body) = Cohttp_lwt_jsoo.Client.call meth (Request.uri request) ~body in
    let status = Cohttp.Response.status response in
    if List.mem (Cohttp.Code.code_of_status status) [0; 502; 503; 504] then
      (
        if attempt >= max_attempts then
          Lwt.return_error @@ ServerUnreachable {request; status}
        else
          let delay = (2. ** (float_of_int attempt) +. Random.float 2.) /. 8. in
          Js_of_ocaml_lwt.Lwt_js.sleep delay;%lwt
          call_retry (attempt + 1)
      )
    else
      let%lwt body = Response.body_of_lwt body in
      lwt_ok (response, body)
  in
  call_retry (if retry then 1 else max_int)

let batch_delay = 0.01 (* 10 ms *)

type batch_state =
  | GatheringBatch of (Request.t * (Response.t, error) result Lwt.u) list
  | Idle

let batch_state = ref Idle

let batch_route = ref None

let initialise_batch_route r =
  match !batch_route with
  | None -> batch_route := Some r
  | Some _ -> failwith "Madge_client: batch route initialised twice"

let batch_route () =
  match !batch_route with
  | None -> failwith "Madge_client: processed a batch, but batch route has not been initialised"
  | Some batch_route -> batch_route

type request_list = Request.t list
[@@deriving of_biniou, yojson]

type response_list = Response.t list
[@@deriving of_biniou, yojson]

let process_batch () =
  match !batch_state with
  | Idle -> assert false
  | GatheringBatch [(request, resolver)] ->
    batch_state := Idle;
    let%lwt response = call_retry request in
    Lwt.wakeup_later resolver response;
    lwt_unit
  | GatheringBatch batch ->
    batch_state := Idle;
    let (requests, resolvers) = List.split (List.rev batch) in
    with_request
      (batch_route ())
      (fun _ batched_request ->
        match%lwt call_retry batched_request with
        | Error error ->
          let batch_error =
            match error with
            | ServerUnreachable details -> ServerUnreachable details
            | Http {request; status; message} -> Http {request; status; message = "Batched request: " ^ message}
            | BodyUnserialisation {body; message} -> BodyUnserialisation {body; message = "Batched request: " ^ message}
          in
          List.iter (fun resolver -> Lwt.wakeup_later resolver (Result.Error batch_error)) resolvers;
          lwt_unit
        | Ok batched_response ->
          let responses = response_list_of_biniou_exn @@ Bi_io.tree_of_string @@ Cohttp.Body.to_string @@ snd batched_response in
          List.iter2
            (fun response resolver -> Lwt.wakeup_later resolver (Ok response))
            responses
            resolvers;
          lwt_unit
      )
      requests

let call_batch (request : Request.t) : (Response.t, error) result Lwt.t =
  let (promise, resolver) = Lwt.wait () in
  let batch_elem = (request, resolver) in
  batch_state :=
    (
      match !batch_state with
      | GatheringBatch batch -> GatheringBatch (batch_elem :: batch)
      | Idle ->
        Lwt.async (fun () -> Js_of_ocaml_lwt.Lwt_js.sleep batch_delay;%lwt process_batch ());
        GatheringBatch [batch_elem]
    );
  promise

(** A very short-lived cache to avoid performing the exact same request several times in a row. *)
let cache = Cache.create ~lifetime: 1 ()

let call_gen
  : type a r z. ?retry: bool ->
  (a, z, r) Route.t ->
  ((r, error) result Lwt.t -> z) ->
  a
= fun ?retry route cont ->
  with_request route @@ fun (module R) request ->
  cont @@
    let%rlwt (response, body) =
      ignore retry;
      (* FIXME *)
      Cache.use ~cache ~key: request ~if_: Request.(is_safe @@ meth request) @@ fun () ->
      call_batch request
    in
    let status = Cohttp.Response.status response in
    let body = Cohttp.Body.to_string body in
    let%rlwt biniou_body =
      try
        Lwt.return_ok @@ Bi_io.tree_of_string body
      with
        | Bi_util.Error message ->
          Lwt.return_error @@ BodyUnserialisation {body; message = "not Biniou: " ^ message}
    in
    if Cohttp.(Code.(is_success (code_of_status status))) then
      (
        match R.of_biniou biniou_body with
        | Error (message, _) -> Lwt.return_error @@ BodyUnserialisation {body; message}
        | Ok body -> Lwt.return_ok body
      )
    else
      (
        match error_response_of_biniou biniou_body with
        | Error (message, _) -> Lwt.return_error @@ BodyUnserialisation {body; message = "expected an error, but " ^ message}
        | Ok {message} -> Lwt.return_error @@ Http {request; status; message}
      )

let call
  : type a r. ?retry: bool -> (a, (r, error) result Lwt.t, r) Route.t -> a
= fun ?retry route -> call_gen ?retry route id

let call_exn
  : type a r. ?retry: bool -> (a, r Lwt.t, r) Route.t -> a
= fun ?retry route -> call_gen ?retry route @@ Lwt.map @@ Result.fold ~ok: Fun.id ~error: (fun e -> raise (Error e))
