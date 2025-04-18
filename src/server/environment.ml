open Nes
open Common

type session = {
  user: Model.User.t Entry.t option;
}
[@@deriving fields]

let make_session () = {
  user = None;
}

type t = {
  session_id: string;
  session: session;
}
[@@deriving fields]

let uid () =
  Printf.sprintf "%Lx" @@
    Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int

let sessions = Hashtbl.create 8

let make ~request () =
  let headers = Cohttp.Request.headers request in
  let cookies =
    match Cohttp.Header.get headers "Cookie" with
    | None -> []
    | Some cookies ->
      let cookies = Str.split (Str.regexp "[ \t]*;[ \t]*") cookies in
      List.filter_map (String.split_on_first_char '=') cookies
  in
  let session_id = Option.value' (List.assoc_opt "session" cookies) ~default: uid in
  let session =
    Option.value'
      (Hashtbl.find_opt sessions session_id)
      ~default: (fun () ->
        let session = make_session () in
        Hashtbl.add sessions session_id session;
        session
      )
  in
    {session_id; session}

let add_session_cookie env headers =
  Cohttp.Header.add headers "Set-Cookie" ("session=" ^ env.session_id)

let update_session env f =
  let new_session = f env.session in
  Hashtbl.replace sessions env.session_id new_session;
  {env with session = new_session}

let set_session_user env user =
  update_session env @@ fun _ -> {user}
