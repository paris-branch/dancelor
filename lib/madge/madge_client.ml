open Madge_common

let add_arg query arg val_ =
  query := (arg_key arg, [Yojson.Safe.to_string (arg_serialiser arg val_)]) :: !query

let add_opt_arg query arg val_ =
  match val_ with
  | None -> ()
  | Some val_ -> query := (arg_key arg, [Yojson.Safe.to_string (arg_serialiser arg val_)]) :: !query

let call ~endpoint query_builder =
  let query = ref [] in
  query_builder query;
  let%lwt (response, body) =
    Cohttp_lwt_xhr.Client.call
      (endpoint_meth endpoint)
      (Uri.make ~path:(!prefix ^ endpoint_path endpoint) ~query:!query ())
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
