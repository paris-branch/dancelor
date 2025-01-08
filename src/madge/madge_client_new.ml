include Madge_common_new

let call : type a r. (a, r Lwt.t, r) Route.t -> a = fun route ->
  Route.process route @@ fun (module R) uri ->
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
