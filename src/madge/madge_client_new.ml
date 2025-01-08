include Madge

(* FIXME: This is not just client, but a very specific type of client. Either
   make it Dancelor-specific, or rename in eg. Madge_yojson_cohttp_client. *)

let call : type a r. (a, r Lwt.t, r) route -> a = fun route ->
  process route @@ fun (module R) uri ->
  let body = Cohttp_lwt.Body.of_string "" in
  let%lwt (response, body) = Cohttp_lwt_jsoo.Client.call `GET uri ~body in
  if Cohttp.(Code.(is_success (code_of_status (Response.status response)))) then
    (
      let%lwt body = Cohttp_lwt.Body.to_string body in
      (* FIXME: error handling of JSON parsing and conversion from JSON *)
      Lwt.return (Result.get_ok @@ R.of_yojson @@ Yojson.Safe.from_string body)
    )
  else
    assert false (* FIXME: error handling *)
