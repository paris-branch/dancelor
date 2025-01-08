include Madge_common_new
open Route

let rec call : type a r. Buffer.t -> (a, r Lwt.t, r) t -> a = fun path route ->
  match route with
  | Return(module R) ->
    let uri = Uri.make ~path: (Buffer.contents path) () in
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
  | Literal (str, route) ->
    Buffer.add_char path '/';
    Buffer.add_string path str;
    call path route
  | Variable (rt, route) ->
    fun x ->
      Buffer.add_char path '/';
      (* FIXME: url encode *)
      Buffer.add_string path (rt.to_string x);
      call path route

let call : type a r. (a, r Lwt.t, r) t -> a = fun route ->
  call (Buffer.create 8) route
