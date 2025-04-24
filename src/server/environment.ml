open NesUnix
open Common

module Log = (val Logger.create "environment": Logs.LOG)

(* A helper to generate unique identifiers that are somewhat readable. It is not
   cryptographically secure, but probably good enough. *)
let uid () =
  Format.sprintf
    "%Lx%Lx"
    (Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int)
    (Random.int64_in_range ~min: Int64.min_int ~max: Int64.max_int)

let session_max_age = 43200 (* 43200 seconds = 12 hours *)
let remember_me_token_max_age = 15552000 (* 15552000 seconds = 6 * 30 days *)

type session = {
  user: Model.User.t Entry.t option;
  expires: Datetime.t;
}
[@@deriving fields]

let make_new_session () =
  {user = None; expires = Datetime.make_in_the_future (float_of_int session_max_age)}

let update_session_expiration session =
  {session with expires = Datetime.make_in_the_future (float_of_int session_max_age)}

type t = {
  session_id: string;
  session: session ref;
  response_cookies: (Cohttp.Header.t -> Cohttp.Header.t) list ref;
}
[@@deriving fields]

let user = user % (!) % session

(* A hash table linking session ids to sessions. *)
let sessions : (string, session) Hashtbl.t = Hashtbl.create 8

let set_user session_id session user =
  session := {!session with user = Some user};
  Hashtbl.replace sessions session_id !session

(* Given an environment, check whether we should log in a user via their
   remember me token. This is meant to be used in {!make}, before returning a
   transient environment. *)
let process_remember_me_cookie session_id session remember_me_cookie =
  match String.split_on_first_char ':' remember_me_cookie with
  | None -> Lwt.return_unit
  | Some (username, token) ->
    Log.info (fun m -> m "Attempt to get remembered with username `%s`." username);
    match Slug.check_string username with
    | None ->
      Log.info (fun m -> m "Rejecting because username is not even a slug.");
      Lwt.return_unit
    | Some username ->
      match%lwt Database.User.get_opt username with
      | None ->
        Log.info (fun m -> m "Rejecting because of wrong username.");
        Lwt.return_unit
      | Some user ->
        match Model.User.remember_me_token user with
        | None ->
          Log.info (fun m -> m "Rejecting because user has no rememberMe token.");
          Lwt.return_unit
        | Some (_, token_max_date) when Datetime.in_the_past token_max_date ->
          Log.info (fun m -> m "Rejecting because token is too old.");
          Lwt.return_unit
        | Some (hashed_token, _) when not @@ HashedPassword.is ~clear: token hashed_token ->
          Log.info (fun m -> m "Rejecting because tokens do not match.");
          Lwt.return_unit
        | Some _ ->
          Log.info (fun m -> m "Accepting login.");
          set_user session_id session user;
          Lwt.return_unit

let from_request request =
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
    ref @@
      match Hashtbl.find_opt sessions session_id with
      | None -> make_new_session ()
      | Some session when Datetime.in_the_past session.expires -> make_new_session ()
      | Some session -> update_session_expiration session
  in
  (
    match !session.user, List.assoc_opt "rememberMe" cookies with
    | None, Some remember_me_cookie ->
      process_remember_me_cookie session_id session remember_me_cookie
    | _ -> Lwt.return_unit
  );%lwt
  let response_cookies = ref [] in
  Lwt.return {session_id; session; response_cookies}

let register_response_cookie env cookie =
  env.response_cookies := cookie :: !(env.response_cookies)

let add_cookie ?max_age ?path ?(secure = false) ?(httpOnly = false) key value headers =
  Cohttp.Header.add headers "Set-Cookie" @@
  (key ^ "=" ^ value) ^
  (match path with None -> "" | Some path -> "; Path=" ^ path) ^
  (match secure with false -> "" | true -> "; Secure") ^
  (match httpOnly with false -> "" | true -> "; HttpOnly") ^
    (match max_age with None -> "" | Some max_age -> "; Max-Age=" ^ string_of_int max_age)

let delete_cookie ?path key headers =
  Cohttp.Header.add headers "Set-Cookie" @@
  (key ^ "=deleted") ^
  (match path with None -> "" | Some path -> "; Path=" ^ path) ^
  "; Expires=Thu, 01 Jan 1970 00:00:00 GMT"

let update_reponse_headers response f =
  let open Cohttp in
  (* FIXME: not super robust if fields get added to the response. *)
  Response.make
    ~version: (Response.version response)
    ~status: (Response.status response)
    ~encoding: (Response.encoding response)
    ~headers: (f @@ Response.headers response)
    ()

let to_response env (response, body) = (
  (
    update_reponse_headers response @@ fun headers ->
    let headers = add_cookie ~path: "/" ~secure: true ~httpOnly: true "session" env.session_id headers in
    List.fold_left (fun headers response_cookie -> response_cookie headers) headers !(env.response_cookies)
  ),
  body
)

let with_ request f =
  let%lwt env = from_request request in
  Lwt.map (to_response env) (f env)

(** Helper to update the user in the database. *)
let database_update_user user f =
  Lwt.map ignore @@ Database.User.update (Entry.slug user) (f @@ Entry.value user)

let login env user ~remember_me =
  set_user env.session_id env.session user;
  Lwt.if_' remember_me (fun () ->
    let token = uid () in
    (
      let hashed_token = HashedPassword.make ~clear: token in
      (* the max date stored in database is one more minute than the max age that
         will be sent as cookie, to be sure not to lie to our clients *)
      let max_date = Datetime.make_in_the_future (float_of_int @@ 60 + remember_me_token_max_age) in
      let remember_me_token = Some (hashed_token, max_date) in
      database_update_user user (fun user -> {user with remember_me_token})
    );%lwt
    register_response_cookie
      env
      (
        add_cookie
          ~path: "/"
          ~secure: true
          ~httpOnly: true
          "rememberMe"
          (Entry.slug_as_string user ^ ":" ^ token)
          ~max_age: remember_me_token_max_age
      );
    Lwt.return_unit
  )

let logout env user =
  let session = {!(env.session) with user = None} in
  Hashtbl.replace sessions env.session_id session;
  env.session := session;
  database_update_user user (fun user -> {user with remember_me_token = None});%lwt
  register_response_cookie env (delete_cookie ~path: "/" "rememberMe");
  Lwt.return_unit
