include Madge

let meth_to_cohttp_code_meth = function
  | GET -> `GET

let call : type a r. (a, r Lwt.t, r) route -> a = fun route ->
  process route @@ fun (module R) {meth; uri; body} ->
  let meth = meth_to_cohttp_code_meth meth in
  let body = Cohttp_lwt.Body.of_string body in
  let%lwt (response, body) = Cohttp_lwt_jsoo.Client.call meth uri ~body in
  if Cohttp.(Code.(is_success (code_of_status (Response.status response)))) then
    (
      let%lwt body = Cohttp_lwt.Body.to_string body in
      (* FIXME: error handling of JSON parsing and conversion from JSON *)
      Lwt.return (Result.get_ok @@ R.of_yojson @@ Yojson.Safe.from_string body)
    )
  else
    assert false (* FIXME: error handling *)
