open Madge_common

type add_arg     = { a : 'a. ('a, mandatory) arg -> 'a -> unit }
type add_opt_arg = { o : 'a. ('a, optional) arg -> 'a option -> unit }

let add_arg query : add_arg =
  let a arg val_ =
    query := (arg_key arg, arg_serialiser arg val_) :: !query
  in
  { a }

let add_opt_arg query : add_opt_arg =
  let o arg val_ =
    match val_ with
    | None -> ()
    | Some val_ -> query := (arg_key arg, arg_serialiser arg val_) :: !query
  in
  { o }

let call ~meth ~uri ~body =
  Cohttp_lwt_jsoo.Client.call meth uri ~body

let call ~endpoint query_builder =
  let query =
    let query = ref [] in
    let add_arg = add_arg query in
    let add_opt_arg = add_opt_arg query in
    query_builder add_arg add_opt_arg;
    Yojson.Safe.to_string (`Assoc !query)
  in
  let%lwt (response, body) =
    call
      ~meth:(endpoint_meth endpoint)
      ~uri:(Uri.make ~path:(!prefix ^ endpoint_path endpoint) ())
      ~body:(`String query)
  in
  if Cohttp.(Code.(is_success (code_of_status (Response.status response)))) then
    (
      let%lwt body = Cohttp_lwt.Body.to_string body in
      let body = Yojson.Safe.from_string body in
      match endpoint_unserialiser endpoint body with
      | Ok x -> Lwt.return x
      | Error _ -> assert false (* FIXME *)
    )
  else
    assert false (* FIXME *)

(* FIXME: handle errors in request and unserialiser *)
