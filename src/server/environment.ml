open NesUnix
open Common

module Log = (val Logger.create "environment": Logs.LOG)

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

let pp fmt env =
  fpf
    fmt
    "%s [%s, expires %a]"
    (
      match !(env.session).user with
      | None -> "<anynomous>"
      | Some user -> Entry.id_as_string user
    )
    env.session_id
    Datetime.pp
    !(env.session).expires

let user = user % (!) % session

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

(* A hash table linking session ids to sessions. *)
let sessions : (string, session) Hashtbl.t = Hashtbl.create 8

let set_user env user =
  env.session := {!(env.session) with user = Some user};
  Hashtbl.replace sessions env.session_id !(env.session)

(** Helper to update the user in the database. *)
let database_update_user user f =
  let%lwt new_user = f @@ Entry.value user in
  ignore <$> Database.User.update (Entry.id user) new_user

(* Given an environment, check whether we should log in a user via their
   remember me token. This is meant to be used in {!make}, before returning a
   transient environment. *)
let process_remember_me_cookie env remember_me_cookie =
  match String.split_3_on_char ':' remember_me_cookie with
  | None -> lwt_unit
  | Some (id, key, token) ->
    Log.info (fun m -> m "Attempt to get remembered with id `%s`." id);
    match Entry.Id.of_string id with
    | None ->
      Log.info (fun m -> m "Rejecting because id is not valid.");
      register_response_cookie env (delete_cookie ~path: "/" "rememberMe");
      lwt_unit
    | Some id ->
      match%lwt Database.User.get id with
      | None ->
        Log.info (fun m -> m "Rejecting because of wrong username.");
        register_response_cookie env (delete_cookie ~path: "/" "rememberMe");
        lwt_unit
      | Some user ->
        let tokens = Model.User.remember_me_tokens' user in
        match String.Map.find_opt key tokens with
        | None ->
          Log.info (fun m -> m "Rejecting because user does not have the “remember me” key `%s`." key);
          register_response_cookie env (delete_cookie ~path: "/" "rememberMe");
          lwt_unit
        | Some (_, token_max_date) when Datetime.in_the_past token_max_date ->
          Log.info (fun m -> m "Rejecting because token is too old.");
          database_update_user user (Model.User.update ~remember_me_tokens: (String.Map.remove key));%lwt
          register_response_cookie env (delete_cookie ~path: "/" "rememberMe");
          lwt_unit
        | Some (hashed_token, _) when not @@ HashedSecret.is ~clear: token hashed_token ->
          Log.info (fun m -> m "Rejecting because tokens do not match.");
          (* someone got their hand on a “remember me” key - invalidate all known tokens *)
          database_update_user user (Model.User.update ~remember_me_tokens: (const String.Map.empty));%lwt
          register_response_cookie env (delete_cookie ~path: "/" "rememberMe");
          lwt_unit
        | Some _ ->
          set_user env user;
          Log.info (fun m -> m "Accepted sign in for %a." pp env);
          lwt_unit

let from_request request =
  Log.debug (fun m -> m "In Environment.from_request...");
  let headers = Cohttp.Request.headers request in
  Log.debug (fun m -> m "Header:@\n%a" Cohttp.Header.pp_hum headers);
  let cookies =
    match Cohttp.Header.get headers "Cookie" with
    | None -> []
    | Some cookies ->
      let cookies = Str.split (Str.regexp "[ \t]*;[ \t]*") cookies in
      List.filter_map (String.split_2_on_char '=') cookies
  in
  Log.debug (fun m -> m "Got cookies: %a" (Format.pp_print_list ~pp_sep: (fun fmt () -> fpf fmt "@\n  - ") (fun fmt (k, v) -> fpf fmt "%s -> %s" k v)) cookies);
  let session_id = Option.value' (List.assoc_opt "session" cookies) ~default: uid in
  Log.debug (fun m -> m "Session id: %s" session_id);
  let session =
    ref @@
      match Hashtbl.find_opt sessions session_id with
      | None -> Log.debug (fun m -> m "No session: creating a new one."); make_new_session ()
      | Some session when Datetime.in_the_past session.expires -> Log.debug (fun m -> m "Old session: creating a new one."); make_new_session ()
      | Some session -> Log.debug (fun m -> m "Live session: updating."); update_session_expiration session
  in
  let response_cookies = ref [] in
  let env = {session_id; session; response_cookies} in
  (
    match !session.user, List.assoc_opt "rememberMe" cookies with
    | None, Some remember_me_cookie ->
      Log.debug (fun m -> m "Processing “remember me” cookie.");
      process_remember_me_cookie env remember_me_cookie
    | _ -> lwt_unit
  );%lwt
  Log.debug (fun m -> m "Environment.from_request done; user is: %a." pp env);
  lwt env

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
  to_response env <$> f env

let sign_in env user ~remember_me =
  set_user env user;
  Lwt.if_' remember_me (fun () ->
    let key = uid () in
    let token = uid () in
    (
      let hashed_token = HashedSecret.make ~clear: token in
      (* the max date stored in database is one more minute than the max age that
         will be sent as cookie, to be sure not to lie to our clients *)
      let max_date = Datetime.make_in_the_future (float_of_int @@ 60 + remember_me_token_max_age) in
      (* let remember_me_token = Some (hashed_token, max_date) in *)
      database_update_user user (Model.User.update ~remember_me_tokens: (String.Map.add key (hashed_token, max_date)))
    );%lwt
    register_response_cookie
      env
      (
        add_cookie
          ~path: "/"
          ~secure: true
          ~httpOnly: true
          "rememberMe"
          (Entry.id_as_string user ^ ":" ^ key ^ ":" ^ token)
          ~max_age: remember_me_token_max_age
      );
    lwt_unit
  )

let sign_out env user =
  let session = {!(env.session) with user = None} in
  Hashtbl.replace sessions env.session_id session;
  env.session := session;
  database_update_user user (Model.User.update ~remember_me_tokens: (const String.Map.empty));%lwt
  register_response_cookie env (delete_cookie ~path: "/" "rememberMe");
  lwt_unit

let boot_time = Datetime.now ()
