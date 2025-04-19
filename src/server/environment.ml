open Nes
open Common

(* A helper to generate unique identifiers that are somewhat readable. It is not
   cryptographically secure, but probably good enough. *)
let uid () =
  Format.sprintf
    "%Lx%Lx"
    (Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int)
    (Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int)

let session_max_age = 43200 (* 43200 seconds = 12 hours *)

type session = {
  user: Model.User.t Entry.t option;
  expires: Datetime.t;
}
[@@deriving fields]

let make_expires () =
  (* One minute of margin to be sure that we are not lying to the client. *)
  Datetime.make_in_the_future (float_of_int session_max_age)

let make_new_session () =
  {user = None; expires = make_expires ()}

let update_session_expiration session =
  {session with expires = make_expires ()}

type t = {
  session_id: string;
  session: session;
}
[@@deriving fields]

let user = user % session

(* A hash table linking session ids to sessions. *)
let sessions : (string, session) Hashtbl.t = Hashtbl.create 8

let make ~request =
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
    match Hashtbl.find_opt sessions session_id with
    | None -> make_new_session ()
    | Some session when Datetime.in_the_past session.expires -> make_new_session ()
    | Some session -> update_session_expiration session
  in
    {session_id; session}

let add_cookies env headers =
  Cohttp.Header.add headers "Set-Cookie" ("session=" ^ env.session_id ^ "; Path=/; Secure; HttpOnly")

let set_user env user =
  let session = {env.session with user} in
  Hashtbl.replace sessions env.session_id session;
  {env with session}
