open Nes

type t = {
  session_id: string;
}

let uid () =
  Printf.sprintf "%Lx" @@
    Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int

let make ~request () =
  let headers = Cohttp.Request.headers request in
  match Cohttp.Header.get headers "Cookie" with
  | None -> {session_id = uid ()}
  | Some cookies ->
    let cookies = Str.split (Str.regexp "[ \t]*;[ \t]*") cookies in
    let cookies = List.filter_map (String.split_on_first_char '=') cookies in
    match List.assoc_opt "session" cookies with
    | None -> {session_id = uid ()}
    | Some session_id -> {session_id}

let add_session_cookie env headers =
  Cohttp.Header.add headers "Set-Cookie" ("session=" ^ env.session_id)
